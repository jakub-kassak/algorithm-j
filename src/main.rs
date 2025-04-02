mod ast;
mod infer;
mod lexer;
mod parser;
mod test;

use infer::{infer, string_of_type, Env, TypeInferenceContext};
use parser::Parser;
use std::collections::HashMap;
use std::vec;

fn check(input: &str, expected: &str) -> bool {
    println!("CHECK:\t{}", input);
    let input = input.trim();
    let mut parser = Parser::new(input);
    let expr = parser.parse();

    let mut ctx = TypeInferenceContext::new();
    let mut map = HashMap::new();
    let mut env = Env::new(&mut map);
    let typ = infer(&mut ctx, &mut env, &expr);
    let str_typ = string_of_type(&typ);
    let res = expected == str_typ;
    println!("\t{}: {}", res, str_typ);
    res
}

fn main() {
    let test_add = vec![
        (r#"1"#, "Int"),
        (r#"let x = 1 in x"#, "Int"),
        (r#"-2 + -1 + -4"#, "Int"),
        (r#"\f. f () + 1"#, "(() -> Int) -> Int"),
        (r#"\f.\x. f () x + 1"#, "(() -> a -> Int) -> a -> Int"),
        (r#"1 + 2"#, "Int"),
        (r#"3 + 4 + 5"#, "Int"),
        (r#"let x = 10 in x + 20"#, "Int"),
        (r#"let x = 1 in let y = 2 in x + y"#, "Int"),
        (r#"let add = \x.\y. x + y in add 3 4"#, "Int"),
        (r#"let add = \x.\y. x + y in add (1 + 2) (3 + 4)"#, "Int"),
        (r#"let x = 5 in let y = x + 10 in y + 15"#, "Int"),
        (r#"let f = \x. x + 1 in f 10 + f 20"#, "Int"),
        (
            r#"let f = \x.x in let x = 1 in let y = 2 in let z = 3 in f z + x + y"#,
            "Int",
        ),
    ];
    let test_let_lam = [
        (r#"let x = 1 in let y = 2 in let z = 3 in x + y + z"#, "Int"),
        (r#"let sum = \x.\y.\z. x + y + z in sum 1 2 3"#, "Int"),
        (r#"let x = let y = () in y in x"#, "()"),
        (r#"let x = () in let x = x in x"#, "()"),
        (r#"let id = \x.x in id id"#, "a -> a"),
        (r#"let x = () in x"#, "()"),
        (r#"\x. x ()"#, "(() -> a) -> a"),
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
    let res_add = test_add
        .iter()
        .all(|(input, expected)| check(input, expected));
    let result = test_let_lam
        .iter()
        .all(|(input, expected)| check(input, expected));
    let test_seq = [
        (r#"1; 2"#, "Int"),
        (r#"let x = 1 in let y = 2 in x + y; x + 1"#, "Int"),
        (
            r#"let f = \x.x in (); let x = 1 in (); let y = 2 in (); let z = 3 in ();f z + x + y; f"#,
            "a -> a",
        ),
        (
            r#"\g.let f = \x.x in (); let x = 1 in (); let y = 2 in (); let z = 3 in ();g z + x + y; g"#,
            "(Int -> Int) -> Int -> Int",
        ),
        (
            r#"\g.let x = 1 in  let y = 2 in  g (x + y); g"#,
            "(Int -> a) -> Int -> a",
        ),
        (
            r#"let x = 1 in (); let x = x + 1 in (); let x = x + x in (); x"#,
            "Int",
        ),
        // (r#"let x = 1 in (); let x = \x.x in (); let x = x + x in (); x"#, "Int"),
    ];
    let res_seq = test_seq
        .iter()
        .all(|(input, expected)| check(input, expected));
    let additional_tests = [
        // Functions and higher-order functions
        (r#"\x. x + 1"#, "Int -> Int"),
        (r#"\x.\y. x + y"#, "Int -> Int -> Int"),
        (r#"let add = \x.\y. x + y in add 2 3"#, "Int"),
        (r#"let apply = \f.\x. f x in apply (\x. x + 1) 5"#, "Int"),
        (
            r#"let compose = \f.\g.\x. f (g x) in compose (\x. x + 1) (\x. x + 2) 3"#,
            "Int",
        ),
        // Nested let expressions
        (r#"let x = 1 in let y = x + 1 in let z = y + 1 in z"#, "Int"),
        (r#"let x = 1 in let y = x + 1 in let z = y + x in z"#, "Int"),
        (
            r#"let f = \x. x + 1 in let g = \y. y + 2 in f (g 3)"#,
            "Int",
        ),
        // Unit type and sequences
        (r#"()"#, "()"),
        (r#"let x = () in x"#, "()"),
        (r#"let x = 1 in (); x"#, "Int"),
        (r#"let x = 1 in let y = () in (); x + 1"#, "Int"),
        (r#"let x = () in (); let y = () in x"#, "()"),
        // // Complex higher-order functions
        (r#"\f.\g.\x. f (g x)"#, "(a -> b) -> (c -> a) -> c -> b"),
        (r#"\f.\x.f (f (f (f (f (f x)))))"#, "(a -> a) -> a -> a"),
        (r#"\f.\g.\x. f (g (g x))"#, "(a -> b) -> (a -> a) -> a -> b"),
        // (r#"\f.\x. f (x x)"#, "(a -> a) -> a -> a"), // Recursive-like structure
    ];

    let res_additional = additional_tests
        .iter()
        .all(|(input, expected)| check(input, expected));
    match res_additional {
        true => println!("✅ All additional tests passed"),
        false => println!("❌ Some additional tests failed"),
    }
    match res_add {
        true => println!("✅ All add tests passed"),
        false => println!("❌ Some add tests failed"),
    }
    match result {
        true => println!("✅ All lam, let tests passed"),
        false => println!("❌ Some lam, let tests failed"),
    }
    match res_seq {
        true => println!("✅ All seq tests passed"),
        false => println!("❌ Some seq tests failed"),
    }
}
