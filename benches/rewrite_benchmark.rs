use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tree_rewriting::{make_tree, Ast};

fn post(ast: &Ast) {
    let mut ir = ast.build();
    while ir.rewrite() {}
    black_box(ir);
}

fn during(ast: &Ast) {
    let ir = ast.build_rewrite();
    black_box(ir);
}

fn during_arena(ast: &Ast) {
    let bump = Bump::new();
    let ir = ast.build_rewrite_ref(&bump);
    black_box(ir);
}

fn criterion_benchmark(c: &mut Criterion) {
    let tree = make_tree("x", "y", 10000);
    c.bench_function("post", |b| b.iter(|| post(&tree)));
    c.bench_function("during", |b| b.iter(|| during(&tree)));
    c.bench_function("during_arena", |b| b.iter(|| during_arena(&tree)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
