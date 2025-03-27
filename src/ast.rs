#[derive(Debug)]
pub enum Expr {
    Unit,
    Int(i64),
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
    Add {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Seq {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}
