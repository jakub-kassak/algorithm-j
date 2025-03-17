use crate::ast::Expr;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::Bound;
use std::rc::Rc;

// --- Type Definitions ---

#[derive(Clone, Debug)]
pub enum Type {
    TUnit,
    TVar(Rc<RefCell<TypeVar>>),
    Fn(Box<Type>, Box<Type>),
}

impl Type {
    pub fn new_fn(a: Type, b: Type) -> Self {
        Type::Fn(Box::new(a), Box::new(b))
    }
}

#[derive(Clone, Debug)]
pub enum TypeVar {
    Bound(Type),
    Unbound(usize, usize), // typevar_id, level
}

impl TypeVar {
    fn get_bound(&self) -> Option<&Type> {
        match self {
            TypeVar::Bound(t) => Some(t),
            _ => None,
        }
    }

    fn get_unbound(&self) -> Option<(usize, usize)> {
        match self {
            TypeVar::Unbound(id, level) => Some((*id, *level)),
            _ => None,
        }
    }

    fn set_level(&mut self, new_level: usize) {
        if let TypeVar::Unbound(_, level) = self {
            *level = new_level;
        }
    }
}

#[derive(Clone, Debug)]
pub struct PolyType {
    typevars: Vec<usize>,
    typ: Type,
}

#[derive(Debug)]
pub struct TypeInferenceContext {
    current_level: usize,
    current_typevar: usize,
}

impl TypeInferenceContext {
    pub fn new() -> Self {
        TypeInferenceContext {
            current_level: 1,
            current_typevar: 0,
        }
    }

    pub fn enter_level(&mut self) {
        self.current_level += 1;
    }

    pub fn exit_level(&mut self) {
        self.current_level -= 1;
    }

    fn newvar(&mut self) -> usize {
        self.current_typevar += 1;
        self.current_typevar
    }

    pub fn newvar_t(&mut self) -> Type {
        let id = self.newvar();
        Type::TVar(Rc::new(RefCell::new(TypeVar::Unbound(
            id,
            self.current_level,
        ))))
    }
}

// Your AST definition
// pub enum Expr {
//     Unit,
//     Ident(String),
//     Let {
//         ident: String,
//         value: Box<Expr>,
//         body: Box<Expr>,
//     },
//     Lam {
//         ident: String,
//         body: Box<Expr>,
//     },
//     App {
//         func: Box<Expr>,
//         arg: Box<Expr>,
//     },
// }

// --- Helper Functions ---

type Env = HashMap<String, PolyType>;

// Instantiates a polytype by replacing its bound type variables with fresh monotypes
fn inst(ctx: &mut TypeInferenceContext, poly: &PolyType) -> Type {
    let mut tbl = HashMap::new();
    for tv in &poly.typevars {
        let new_t = ctx.newvar_t();
        tbl.insert(*tv, new_t);
    }
    replace_tvs(&tbl, &poly.typ)
}

// Replaces type variables in a type according to a substitution table
fn replace_tvs(tbl: &HashMap<usize, Type>, t: &Type) -> Type {
    match t {
        Type::TUnit => Type::TUnit,
        Type::TVar(tv) => {
            if let Some(inner_t) = tv.borrow().get_bound() {
                replace_tvs(tbl, inner_t)
            } else if let Some((n, _)) = tv.borrow().get_unbound() {
                tbl.get(&n)
                    .cloned()
                    .unwrap_or_else(|| Type::TVar(tv.clone()))
            } else {
                unreachable!()
            }
        }
        Type::Fn(a, b) => {
            let a_new = replace_tvs(tbl, a);
            let b_new = replace_tvs(tbl, b);
            Type::Fn(Box::new(a_new), Box::new(b_new))
        }
    }
}

// Checks if a type variable occurs in a type, updating levels as needed
fn occurs(a_id: usize, a_level: usize, t: &Type) -> bool {
    match t {
        Type::TUnit => false,
        Type::TVar(tv) => {
            if let Some(inner_t) = tv.borrow().get_bound() {
                return occurs(a_id, a_level, inner_t);
            }
            let tv_unboud = tv.borrow().get_unbound();
            if let Some((b_id, b_level)) = tv_unboud {
                let min_level = a_level.min(b_level);
                tv.borrow_mut().set_level(min_level);
                return b_id == a_id;
            };
            unreachable!()
        }
        Type::Fn(arg, ret) => occurs(a_id, a_level, arg) || occurs(a_id, a_level, ret),
    }
}

// Unifies two types, mutating type variables as needed
fn unify(t1: &Type, t2: &Type) {
    if std::ptr::eq(t1, t2) {
        return;
    }
    match (t1, t2) {
        (Type::TUnit, Type::TUnit) => (),
        (Type::TVar(tv1), _) => {
            let (a_id, a_level) = match *tv1.borrow() {
                TypeVar::Bound(ref t) => {
                    return unify(t, t2);
                }
                TypeVar::Unbound(a_id, a_level) => (a_id, a_level),
            };
            if occurs(a_id, a_level, t2) {
                panic!("TypeError: occurs check failed");
            } else {
                *tv1.borrow_mut() = TypeVar::Bound(t2.clone());
            }
        }
        (_, Type::TVar(tv2)) => {
            let mut tv2_borrow = tv2.borrow_mut();
            if let Some(inner_t) = tv2_borrow.get_bound() {
                unify(t1, inner_t);
            } else if let TypeVar::Unbound(b_id, b_level) = *tv2_borrow {
                if occurs(b_id, b_level, t1) {
                    panic!("TypeError: occurs check failed");
                } else {
                    *tv2_borrow = TypeVar::Bound(t1.clone());
                }
            } else {
                unreachable!()
            }
        }
        (Type::Fn(a, b), Type::Fn(c, d)) => {
            unify(a, c);
            unify(b, d);
        }
        _ => panic!("TypeError: cannot unify"),
    }
}

// Generalizes a type by collecting monomorphic type variables
fn generalize(ctx: &TypeInferenceContext, t: &Type) -> PolyType {
    let typevars = find_all_tvs(ctx, t).into_iter().collect();
    PolyType {
        typevars,
        typ: t.clone(),
    }
}

fn find_all_tvs(ctx: &TypeInferenceContext, t: &Type) -> HashSet<usize> {
    let mut set = HashSet::new();
    find_all_tvs_helper(&mut set, ctx.current_level, t);
    set
}

fn find_all_tvs_helper(set: &mut HashSet<usize>, current_level: usize, t: &Type) {
    match t {
        Type::TUnit => {}
        Type::TVar(tv) => {
            if let Some(inner_t) = tv.borrow().get_bound() {
                find_all_tvs_helper(set, current_level, inner_t);
            } else if let Some((id, level)) = tv.borrow().get_unbound() {
                if level > current_level {
                    set.insert(id);
                }
            }
        }
        Type::Fn(a, b) => {
            find_all_tvs_helper(set, current_level, a);
            find_all_tvs_helper(set, current_level, b);
        }
    }
}

// Creates a polytype without generalization (for lambda parameters)
fn dont_generalize(t: Type) -> PolyType {
    PolyType {
        typevars: vec![],
        typ: t,
    }
}

// Main type inference function
pub fn infer(ctx: &mut TypeInferenceContext, env: &Env, expr: &Expr) -> Type {
    match expr {
        Expr::Unit => Type::TUnit,
        // Var
        //  x : s ∊ env
        //  t = inst s
        //  -----------
        //  infer env x = t
        //
        Expr::Var(x) => {
            let s = env.get(x).expect("Variable not found");
            inst(ctx, s)
        }
        // App
        //  infer env f = t0
        //  infer env x = t1
        //  t' = newvar ()
        //  unify t0 (t1 -> t')
        //  ---------------
        //  infer env (f x) = t'
        //
        Expr::App { func: fun, arg } => {
            let ty_fun = infer(ctx, env, fun);
            let ty_arg = infer(ctx, env, arg);
            let t_fn = Type::new_fn(ty_arg, ctx.newvar_t());
            unify(&ty_fun, &t_fn);
            if let Type::Fn(_, ty_res) = t_fn {
                *ty_res
            } else {
                unreachable!()
            match t_fn {
                Type::Fn(_, b) => *b,
                _ => panic!("TypeError: expected function type"),
            }
            }
        }
        // Abs
        //  t = newvar ()
        //  infer (SMap.add x t env) e = t'
        //  -------------
        //  infer env (fun x -> e) = t -> t'
        //
        Expr::Lam { ident, body } => {
            let t = ctx.newvar_t();
            let poly_t = dont_generalize(t.clone());
            let mut new_env = env.clone();
            new_env.insert(ident.clone(), poly_t);
            let t_prime = infer(ctx, &new_env, body);
            Type::Fn(Box::new(t), Box::new(t_prime))
        }
        // Let
        //  infer env e0 = t
        //  infer (SMap.add x (generalize t) env) e1 = t'
        //  -----------------
        //  infer env (let x = e0 in e1) = t'
        //
        // enter/exit_level optimizations are from
        // http://okmij.org/ftp/ML/generalization.html
        // In this implementation, they're required so we
        // don't generalize types that escape into the environment.
        //
        Expr::Let { ident, value, body } => {
            ctx.enter_level();
            let t = infer(ctx, env, value);
            ctx.exit_level();
            let poly_t = generalize(ctx, &t);
            let mut new_env = env.clone();
            new_env.insert(ident.clone(), poly_t);
            infer(ctx, &new_env, body)
        }
    }
}

// --- Pretty Printing ---

pub fn string_of_type(t: &Type) -> String {
    let mut name_map = HashMap::new();
    let mut next_name = 'a';
    string_of_type_helper(t, &mut name_map, &mut next_name).0
}

fn string_of_type_helper(
    t: &Type,
    name_map: &mut HashMap<usize, String>,
    next_name: &mut char,
) -> (String, bool) {
    match t {
        Type::TUnit => ("unit".to_string(), false),
        Type::TVar(tv) => {
            if let Some(inner_t) = tv.borrow().get_bound() {
                string_of_type_helper(inner_t, name_map, next_name)
            } else if let Some((id, _)) = tv.borrow().get_unbound() {
                let name = name_map
                    .entry(id)
                    .or_insert_with(|| {
                        let name = next_name.to_string();
                        *next_name = char::from_u32(*next_name as u32 + 1).unwrap_or('a');
                        name
                    })
                    .clone();
                (name, false)
            } else {
                unreachable!()
            }
        }
        Type::Fn(a, b) => {
            let (a_str, a_has_fn) = string_of_type_helper(a, name_map, next_name);
            let (b_str, _) = string_of_type_helper(b, name_map, next_name);
            if a_has_fn {
                (format!("({}) -> {}", a_str, b_str), true)
            } else {
                (format!("{} -> {}", a_str, b_str), true)
            }
            // (format!("({} -> {})", a_str, b_str), true)
        }
    }
}
