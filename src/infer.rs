use crate::ast::Expr;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;


// --- Type Definitions ---

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unit,
    Int,
    // A reference to a bound or unbound typevar, set during unification.
    // This is unique to algorithm J where mutation is needed to remember
    // some substitutions.
    TRef(Rc<RefCell<TypeVar>>),
    // 'a -> 'b, all functions are single-argument only
    Fn(Box<Type>, Box<Type>),
}

impl Type {
    pub fn new_fn(a: Type, b: Type) -> Self {
        Type::Fn(Box::new(a), Box::new(b))
    }

    fn is_int(&self) -> bool {
        match self {
            Type::Int => true,
            Type::TRef(tv) => match &*tv.borrow() {
                TypeVar::Link(inner_t) => inner_t.is_int(),
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeVar {
    Link(Type),
    Unbound { id: usize, level: usize },
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
        Type::TRef(Rc::new(RefCell::new(TypeVar::Unbound {
            id,
            level: self.current_level,
        })))
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

#[derive(Debug)]
pub struct Env<'a> {
    map: &'a mut HashMap<String, PolyType>,
    modifications: Vec<(String, Option<PolyType>)>,
}

impl<'a> Env<'a> {
    pub fn new(map: &'a mut HashMap<String, PolyType>) -> Self {
        Env {
            map,
            modifications: vec![],
        }
    }

    fn get(&self, key: &str) -> Option<&PolyType> {
        self.map.get(key)
    }

    fn insert(&mut self, key: String, value: PolyType) {
        let old_value = self.map.insert(key.clone(), value);
        self.modifications.push((key, old_value));
    }

    fn child(&mut self) -> Env<'_> {
        Env {
            map: self.map,
            modifications: vec![],
        }
    }
}

impl Drop for Env<'_> {
    fn drop(&mut self) {
        for (key, old_value) in self.modifications.drain(..) {
            match old_value {
                Some(value) => {
                    self.map.insert(key, value);
                }
                None => {
                    self.map.remove(&key);
                }
            }
        }
    }
}

// type Env = HashMap<String, PolyType>;

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
        Type::Unit => Type::Unit,
        Type::Int => Type::Int,
        Type::TRef(tv) => match &*tv.borrow() {
            TypeVar::Link(inner_t) => replace_tvs(tbl, inner_t),
            TypeVar::Unbound { id, level: _ } => {
                let result = tbl
                    .get(id)
                    .cloned()
                    .unwrap_or_else(|| Type::TRef(tv.clone()));
                result
            }
        },
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
        Type::Unit => false,
        Type::Int => false,
        Type::TRef(tv) => match &mut *tv.borrow_mut() {
            TypeVar::Link(t2) => occurs(a_id, a_level, t2),
            TypeVar::Unbound {
                id: b_id,
                level: b_level,
            } => {
                let min_level = a_level.min(*b_level);
                *b_level = min_level;
                *b_id == a_id
            }
        },
        Type::Fn(arg, ret) => {
            let arg_result = occurs(a_id, a_level, arg);
            let ret_result = occurs(a_id, a_level, ret);
            arg_result || ret_result
        }
    }
}

// Unifies two types, mutating type variables as needed
fn unify(t1: &Type, t2: &Type) {
    if std::ptr::eq(t1, t2) {
        return;
    }

    match (t1, t2) {
        (Type::Unit, Type::Unit) => (),
        (Type::Int, Type::Int) => (),
        (Type::TRef(tv1), _) => {
            if t1 == t2 {
                return;
            }
            let (a_id, a_level) = match *tv1.borrow() {
                TypeVar::Link(ref t) => {
                    return unify(t, t2);
                }
                TypeVar::Unbound {
                    id: a_id,
                    level: a_level,
                } => (a_id, a_level),
            };
            if occurs(a_id, a_level, t2) {
                panic!("TypeError: occurs check failed");
            } else {
                *tv1.borrow_mut() = TypeVar::Link(t2.clone());
            }
        }
        (_, Type::TRef(_tv2)) => {
            unify(t2, t1);
        }
        (Type::Fn(a, b), Type::Fn(c, d)) => {
            unify(c, a);
            unify(d, b);
        }
        _ => {
            panic!("TypeError: cannot unify");
        }
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
        Type::Unit => (),
        Type::Int => (),
        Type::TRef(tv) => match &*tv.borrow() {
            TypeVar::Link(inner_t) => find_all_tvs_helper(set, current_level, inner_t),
            TypeVar::Unbound { id, level } => {
                if *level > current_level {
                    set.insert(*id);
                }
            }
        },
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
pub fn infer(ctx: &mut TypeInferenceContext, env: &mut Env, expr: &Expr) -> Type {
    match expr {
        // Var
        //  x : s ∊ env
        //  t = inst s
        //  -----------
        //  infer env x = t
        Expr::Unit => Type::Unit,
        Expr::Int(_) => Type::Int,
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
        Expr::App { func: e0, arg: e1 } => {
            let t0 = infer(ctx, env, e0);
            let t1 = infer(ctx, env, e1);
            let t_fn = Type::new_fn(t1, ctx.newvar_t());
            unify(&t0, &t_fn);
            if let Type::Fn(_, ty_res) = t_fn {
                *ty_res
            } else {
                unreachable!();
            }
        }
        // Abs (Lambda)
        //  t = newvar ()
        //  infer (SMap.add x t env) e = t'
        //  -------------
        //  infer env (fun x -> e) = t -> t'
        Expr::Lam { ident, body } => {
            let t = ctx.newvar_t();
            let poly_t: PolyType = dont_generalize(t.clone());
            let new_env = &mut env.child();
            new_env.insert(ident.clone(), poly_t);
            let t_prime = infer(ctx, new_env, body);
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
        Expr::Let { ident, value, body } => {
            ctx.enter_level();
            let t = infer(ctx, env, value);
            ctx.exit_level();
            let poly_t = generalize(ctx, &t);
            let new_env = &mut env.child();
            new_env.insert(ident.clone(), poly_t);
            infer(ctx, new_env, body)
        }
        Expr::Add { lhs, rhs } => {
            let t0 = infer(ctx, env, lhs);
            let t1 = infer(ctx, env, rhs);
            unify(&t0, &t1);
            if t0.is_int() && t1.is_int() {
                return Type::Int;
            }
            if let Type::TRef(tv) = t1 {
                let mut tv = tv.borrow_mut();
                if let TypeVar::Unbound { id: _, level: _ } = *tv {
                    *tv = TypeVar::Link(Type::Int);
                    return Type::Int;
                }
            }
            panic!();
        }
        Expr::Seq { lhs, rhs } => {
            infer(ctx, env, lhs);
            infer(ctx, env, rhs)
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
        Type::Unit => ("()".to_string(), false),
        Type::Int => ("Int".to_string(), false),
        Type::TRef(tv) => match &*tv.borrow() {
            TypeVar::Link(inner_t) => string_of_type_helper(inner_t, name_map, next_name),
            TypeVar::Unbound { id, level: _ } => {
                let name = name_map
                    .entry(*id)
                    .or_insert_with(|| {
                        let name = next_name.to_string();
                        *next_name = char::from_u32(*next_name as u32 + 1).unwrap_or('a');
                        name
                    })
                    .clone();
                (name, false)
            }
        },
        Type::Fn(a, b) => {
            let (a_str, a_has_fn) = string_of_type_helper(a, name_map, next_name);
            let (b_str, _) = string_of_type_helper(b, name_map, next_name);
            match a_has_fn {
                true => (format!("({}) -> {}", a_str, b_str), true),
                false => (format!("{} -> {}", a_str, b_str), true),
            }
        }
    }
}
