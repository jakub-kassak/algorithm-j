mod ast;
mod infer;
mod lexer;
mod parser;

use infer::{infer, string_of_type, TypeInferenceContext};
use parser::Parser;
use std::collections::HashMap;
use std::vec;

fn check(input: &str) {
    println!("CHECK:\t{}", input);
    let input = input.trim();
    let mut parser = Parser::new(input);
    let expr = parser.parse();
    // println!("\t{:?}", expr);

    let mut ctx = TypeInferenceContext::new();
    let env = HashMap::new();
    let typ = infer(&mut ctx, &env, &expr);
    // println!("{:#?}", typ);
    println!("\t{}", string_of_type(&typ));
}

fn main() {
    // \x.x x
    let a = vec![
        r#"\n.\f.\x. n f x"#,
        r#"let x = () in x"#,
        r#"\x.\y.x"#,
        r#"\f.\x. f x"#,
        r#"let id = \x.x in id id"#,
        r#"\f.\x. f x"#,
        r#"\f.\x. f (f x)"#,
        r#"\m.\n.\f.\x. m f (n f x)"#,
        r#"\n.\f.\x. f (n f x)"#,
        r#"\m.\n.\f.\x. m (n f) x"#,
        r#"\n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)"#,
        r#"\x. let y = x in y"#, //      : 'a -> 'a
        r#"\x. let y = \z.x in y"#, //   : 'a -> 'b -> 'a
    ];
    for i in a {
        check(i);
    }
}
