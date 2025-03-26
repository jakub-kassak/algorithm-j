mod ast;
mod infer;
mod lexer;
mod parser;

use infer::{infer, string_of_type, TypeInferenceContext};
use parser::Parser;
use std::collections::HashMap;
use std::vec;

fn check(input: &str, expected: &str) -> bool {
    println!("CHECK:\t{}", input);
    let input = input.trim();
    let mut parser = Parser::new(input);
    let expr = parser.parse();

    let mut ctx = TypeInferenceContext::new();
    let mut env = HashMap::new();
    let typ = infer(&mut ctx, &mut env, &expr);
    let str_typ = string_of_type(&typ);
    let res = expected == str_typ;
    println!("\t{}: {}", res, str_typ);
    res
}

fn main() {
    let a = vec![
        (r#"let id = \x.x in id id"#, "a -> a"),
        (r#"\x. x"#, "a -> a"),
        (r#"let x = () in x"#, "unit"),
        (r#"\n.\f.\x. n f x"#, "(a -> b -> c) -> a -> b -> c"),
        (r#"\x.\y.x"#, "a -> b -> a"),
        (r#"\f.\x. f x"#, "(a -> b) -> a -> b"),
        (r#"\f.\x. f (f x)"#, "(a -> a) -> a -> a"),
        (
            r#"\m.\n.\f.\x. m f (n f x)"#,
            "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c",
        ),
        (
            r#"\n.\f.\x. f (n f x)"#,
            "((a -> b) -> c -> a) -> (a -> b) -> c -> b",
        ),
        (
            r#"\m.\n.\f.\x. m (n f) x"#,
            "(a -> b -> c) -> (d -> a) -> d -> b -> c",
        ),
        (
            r#"\n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)"#,
            "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g",
        ),
        (r#"\x. let y = x in y"#, "a -> a"),
        (r#"\x. let y = \z.x in y"#, "a -> b -> a"),
    ];
    let result = a.iter().all(|(input, expected)| check(input, expected));
    match result {
        true => println!("✅ All tests passed"),
        false => println!("❌ Some tests failed"),
    }
}
