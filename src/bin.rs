use bumpalo::Bump;
use tree_rewriting::make_tree;

fn main() {
    let bump = Bump::new();
    let tree = make_tree("x", "y", 100);
    let ir = tree.build_rewrite_ref(&bump);
    // while ir.rewrite() {}
    println!("{:?}", ir);
}
