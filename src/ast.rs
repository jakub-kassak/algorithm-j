#[derive(Debug)]
pub enum Expr {
    Unit,
    Var(String),
    Let {
        ident: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Lam {
        ident: String,
        body: Box<Expr>,
    },
    App {
        func: Box<Expr>,
        arg: Box<Expr>,
    },
}
