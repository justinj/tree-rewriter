use bumpalo::Bump;

#[derive(Debug)]
pub enum Ast<'a> {
    Var(i64, &'a str),
    Int(i64),
    Plus(Box<Ast<'a>>, Box<Ast<'a>>),
}

impl<'a> Ast<'a> {
    pub fn build(&self) -> Ir<'_> {
        match self {
            Ast::Int(i) => Ir::Int(*i),
            Ast::Var(c, v) => Ir::Var(*c, *v),
            Ast::Plus(l, r) => Ir::Plus(Box::new(l.build()), Box::new(r.build())),
        }
    }

    pub fn build_rewrite(&self) -> Ir<'_> {
        match self {
            Ast::Int(i) => Ir::int(*i),
            Ast::Var(c, v) => Ir::var(*c, v),
            Ast::Plus(l, r) => Ir::plus(l.build_rewrite(), r.build_rewrite()),
        }
    }

    pub fn build_rewrite_ref(&self, arena: &'a Bump) -> &'a IrRef<'a> {
        match self {
            Ast::Int(i) => IrRef::int(arena, *i),
            Ast::Var(c, v) => IrRef::var(arena, *c, v),
            Ast::Plus(l, r) => IrRef::plus(
                arena,
                l.build_rewrite_ref(arena),
                r.build_rewrite_ref(arena),
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum IrRef<'a> {
    Var(i64, &'a str),
    Int(i64),
    Plus(&'a IrRef<'a>, &'a IrRef<'a>),
}

impl<'a> IrRef<'a> {
    fn int(arena: &Bump, i: i64) -> &Self {
        arena.alloc(IrRef::Int(i))
    }

    fn var(arena: &'a Bump, i: i64, name: &'a str) -> &'a Self {
        if i == 0 {
            IrRef::int(arena, 0)
        } else {
            arena.alloc(IrRef::Var(i, name))
        }
    }

    fn plus(arena: &'a Bump, x: &'a IrRef<'a>, y: &'a IrRef<'a>) -> &'a Self {
        match (x, y) {
            (IrRef::Int(0), y) => y,
            (x, IrRef::Int(0)) => x,
            (IrRef::Int(x), IrRef::Int(y)) => Self::int(arena, x + y),
            (IrRef::Plus(x, y), z) => Self::plus(arena, *x, Self::plus(arena, *y, z)),
            (x, y) if y < x => Self::plus(arena, y, x),
            (x, IrRef::Plus(y, z)) if *y < x => Self::plus(arena, *y, Self::plus(arena, x, *z)),
            (IrRef::Var(x, c), IrRef::Var(y, d)) if c == d => IrRef::var(arena, x + y, c),
            (x, y) => {
                if let (IrRef::Var(x, c), IrRef::Plus(IrRef::Var(y, d), z)) = (x, y) {
                    if c == d {
                        return IrRef::plus(arena, IrRef::var(arena, *x + y, c), z);
                    }
                }
                arena.alloc(IrRef::Plus(x, y))
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Ir<'a> {
    Var(i64, &'a str),
    Int(i64),
    Plus(Box<Ir<'a>>, Box<Ir<'a>>),
}

impl<'a> Ir<'a> {
    fn int(i: i64) -> Self {
        Ir::Int(i)
    }

    fn var(i: i64, name: &'a str) -> Self {
        if i == 0 {
            Ir::int(0)
        } else {
            Ir::Var(i, name)
        }
    }

    fn plus(mut x: Ir<'a>, mut y: Ir<'a>) -> Self {
        match (x, y) {
            (Ir::Int(0), y) => y,
            (x, Ir::Int(0)) => x,
            (Ir::Int(x), Ir::Int(y)) => Self::int(x + y),
            (Ir::Plus(x, y), z) => Self::plus(*x, Self::plus(*y, z)),
            (x, y) if y < x => Self::plus(y, x),
            (x, Ir::Plus(y, z)) if *y < x => Self::plus(*y, Self::plus(x, *z)),
            (Ir::Var(x, c), Ir::Var(y, d)) if c == d => Ir::var(x + y, c),
            (mut x, mut y) => {
                if let (Ir::Var(x, c), Ir::Plus(y, z)) = (&mut x, &mut y) {
                    if let Ir::Var(y, d) = **y {
                        if *c == d {
                            return Ir::plus(Ir::var(*x + y, c), z.take());
                        }
                    }
                }
                Ir::Plus(Box::new(x), Box::new(y))
            }
        }
    }

    fn take(&mut self) -> Self {
        std::mem::replace(self, Ir::Int(0))
    }

    pub fn rewrite(&mut self) -> bool {
        match self {
            Ir::Plus(l, r) => {
                let mut rewrote = false;

                while l.rewrite() {
                    rewrote = true;
                }
                while r.rewrite() {
                    rewrote = true;
                }

                let inner = match (&mut **l, &mut **r) {
                    (Ir::Int(0), x) => {
                        *self = x.take();
                        true
                    }
                    (x, Ir::Int(0)) => {
                        *self = x.take();
                        true
                    }
                    (Ir::Int(x), Ir::Int(y)) => {
                        *self = Ir::Int(*x + *y);
                        true
                    }
                    (Ir::Plus(x, y), z) => {
                        *self = Ir::Plus(
                            Box::new(x.take()),
                            Box::new(Ir::Plus(Box::new(y.take()), Box::new(z.take()))),
                        );
                        true
                    }
                    (x, y) if *y < *x => {
                        std::mem::swap(x, y);
                        true
                    }
                    (x, Ir::Plus(y, _)) if **y < *x => {
                        std::mem::swap(x, y);
                        true
                    }
                    (Ir::Var(x, c), Ir::Plus(y, z)) => {
                        if let Ir::Var(y, d) = **y {
                            if *c == d {
                                *self = Ir::Plus(Box::new(Ir::Var(*x + y, c)), Box::new(z.take()));
                                return true;
                            }
                        }
                        false
                    }
                    _ => false,
                };
                inner || rewrote
            }
            Ir::Int(_) => false,
            Ir::Var(0, _) => {
                *self = Ir::Int(0);
                true
            }
            Ir::Var(_, _) => false,
        }
    }
}

pub fn make_tree<'a>(v1: &'a str, v2: &'a str, depth: i64) -> Ast<'a> {
    let mut tree = Ast::Int(1);

    for i in 0..depth {
        tree = Ast::Plus(
            Box::new(tree),
            Box::new(match i % 3 {
                0 => Ast::Int(i),
                1 => Ast::Var(1, v1),
                2 => Ast::Var(1, v2),
                _ => unreachable!(),
            }),
        )
    }

    tree
}
