#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        infer::{infer, string_of_type, Env, TypeInferenceContext},
        parser::Parser,
    };

    fn check(input: &str, expected: &str) -> bool {
        println!("CHECK:\t{}", input);
        let input = input.trim();
        let mut parser = Parser::new(input);
        let expr = parser.parse();
        println!("\t{:?}", expr);

        let mut ctx = TypeInferenceContext::new();
        let mut map = HashMap::new();
        let mut env = Env::new(&mut map);
        let typ = infer(&mut ctx, &mut env, &expr);
        let str_typ = string_of_type(&typ);
        let res = expected == str_typ;
        println!("\t{}: {}", res, str_typ);
        res
    }

    #[test]
    fn test_1() {
        assert!(check(r#"1"#, "Int"));
    }

    #[test]
    fn test_let_x_in_x() {
        assert!(check(r#"let x = 1 in x"#, "Int"));
    }

    #[test]
    fn test_negative_addition() {
        assert!(check(r#"-2 + -1 + -4"#, "Int"));
    }

    #[test]
    fn test_function_application() {
        assert!(check(r#"\f. f () + 1"#, "(() -> Int) -> Int"));
    }

    #[test]
    fn test_nested_function_application() {
        assert!(check(
            r#"\f.\x. f () x + 1"#,
            "(() -> a -> Int) -> a -> Int"
        ));
    }

    #[test]
    fn test_simple_addition() {
        assert!(check(r#"1 + 2"#, "Int"));
    }

    #[test]
    fn test_chained_addition() {
        assert!(check(r#"3 + 4 + 5"#, "Int"));
    }

    #[test]
    fn test_let_x_in_x_plus_20() {
        assert!(check(r#"let x = 10 in x + 20"#, "Int"));
    }

    #[test]
    fn test_nested_let_bindings() {
        assert!(check(r#"let x = 1 in let y = 2 in x + y"#, "Int"));
    }

    #[test]
    fn test_let_add_function() {
        assert!(check(r#"let add = \x.\y. x + y in add 3 4"#, "Int"));
    }

    #[test]
    fn test_let_add_with_expressions() {
        assert!(check(
            r#"let add = \x.\y. x + y in add (1 + 2) (3 + 4)"#,
            "Int"
        ));
    }

    #[test]
    fn test_let_y_in_y_plus_15() {
        assert!(check(r#"let x = 5 in let y = x + 10 in y + 15"#, "Int"));
    }

    #[test]
    fn test_let_f_function() {
        assert!(check(r#"let f = \x. x + 1 in f 10 + f 20"#, "Int"));
    }

    #[test]
    fn test_complex_let_bindings() {
        assert!(check(
            r#"let f = \x.x in let x = 1 in let y = 2 in let z = 3 in f z + x + y"#,
            "Int"
        ));
    }
    #[test]
    fn test_let_lam_1() {
        assert!(check(
            r#"let x = 1 in let y = 2 in let z = 3 in x + y + z"#,
            "Int"
        ));
    }

    #[test]
    fn test_let_lam_2() {
        assert!(check(
            r#"let sum = \x.\y.\z. x + y + z in sum 1 2 3"#,
            "Int"
        ));
    }

    #[test]
    fn test_let_lam_3() {
        assert!(check(r#"let x = let y = () in y in x"#, "()"));
    }

    #[test]
    fn test_let_lam_4() {
        assert!(check(r#"let x = () in let x = x in x"#, "()"));
    }

    #[test]
    fn test_let_lam_5() {
        assert!(check(r#"let id = \x.x in id id"#, "a -> a"));
    }

    #[test]
    fn test_let_lam_6() {
        assert!(check(r#"let x = () in x"#, "()"));
    }

    #[test]
    fn test_let_lam_7() {
        assert!(check(r#"\x. x ()"#, "(() -> a) -> a"));
    }

    #[test]
    fn test_let_lam_8() {
        assert!(check(r#"\n.\f.\x. n f x"#, "(a -> b -> c) -> a -> b -> c"));
    }

    #[test]
    fn test_let_lam_9() {
        assert!(check(r#"\x.\y.x"#, "a -> b -> a"));
    }

    #[test]
    fn test_let_lam_10() {
        assert!(check(r#"\f.\x. f x"#, "(a -> b) -> a -> b"));
    }

    #[test]
    fn test_let_lam_11() {
        assert!(check(r#"\f.\x. f (f x)"#, "(a -> a) -> a -> a"));
    }

    #[test]
    fn test_let_lam_12() {
        assert!(check(
            r#"\m.\n.\f.\x. m f (n f x)"#,
            "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c"
        ));
    }

    #[test]
    fn test_let_lam_13() {
        assert!(check(
            r#"\n.\f.\x. f (n f x)"#,
            "((a -> b) -> c -> a) -> (a -> b) -> c -> b"
        ));
    }

    #[test]
    fn test_let_lam_14() {
        assert!(check(
            r#"\m.\n.\f.\x. m (n f) x"#,
            "(a -> b -> c) -> (d -> a) -> d -> b -> c"
        ));
    }

    #[test]
    fn test_let_lam_15() {
        assert!(check(
            r#"\n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)"#,
            "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g"
        ));
    }

    #[test]
    fn test_let_lam_16() {
        assert!(check(r#"\x. let y = x in y"#, "a -> a"));
    }

    #[test]
    fn test_let_lam_17() {
        assert!(check(r#"\x. let y = \z.x in y"#, "a -> b -> a"));
    }

    #[test]
    fn test_seq_1() {
        assert!(check(r#"1; 2"#, "Int"));
    }

    #[test]
    fn test_seq_2() {
        assert!(check(r#"let x = 1 in let y = 2 in x + y; x + 1"#, "Int"));
    }

    #[test]
    fn test_seq_3() {
        assert!(check(
            r#"let f = \x.x in (); let x = 1 in (); let y = 2 in (); let z = 3 in ();f z + x + y; f"#,
            "a -> a"
        ));
    }

    #[test]
    fn test_seq_4() {
        assert!(check(
            r#"\g.let f = \x.x in (); let x = 1 in (); let y = 2 in (); let z = 3 in ();g z + x + y; g"#,
            "(Int -> Int) -> Int -> Int"
        ));
    }

    #[test]
    fn test_seq_5() {
        assert!(check(
            r#"\g.let x = 1 in  let y = 2 in  g (x + y); g"#,
            "(Int -> a) -> Int -> a"
        ));
    }

    #[test]
    fn test_seq_6() {
        assert!(check(
            r#"let x = 1 in (); let x = x + 1 in (); let x = x + x in (); x"#,
            "Int"
        ));
    }

    #[test]
    fn test_additional_1() {
        assert!(check(r#"\x. x + 1"#, "Int -> Int"));
    }

    #[test]
    fn test_additional_2() {
        assert!(check(r#"\x.\y. x + y"#, "Int -> Int -> Int"));
    }

    #[test]
    fn test_additional_3() {
        assert!(check(r#"let add = \x.\y. x + y in add 2 3"#, "Int"));
    }

    #[test]
    fn test_additional_4() {
        assert!(check(
            r#"let apply = \f.\x. f x in apply (\x. x + 1) 5"#,
            "Int"
        ));
    }

    #[test]
    fn test_additional_5() {
        assert!(check(
            r#"let compose = \f.\g.\x. f (g x) in compose (\x. x + 1) (\x. x + 2) 3"#,
            "Int"
        ));
    }

    #[test]
    fn test_additional_6() {
        assert!(check(
            r#"let x = 1 in let y = x + 1 in let z = y + 1 in z"#,
            "Int"
        ));
    }

    #[test]
    fn test_additional_7() {
        assert!(check(
            r#"let x = 1 in let y = x + 1 in let z = y + x in z"#,
            "Int"
        ));
    }

    #[test]
    fn test_additional_8() {
        assert!(check(
            r#"let f = \x. x + 1 in let g = \y. y + 2 in f (g 3)"#,
            "Int"
        ));
    }

    #[test]
    fn test_additional_9() {
        assert!(check(r#"()"#, "()"));
    }

    #[test]
    fn test_additional_10() {
        assert!(check(r#"let x = () in x"#, "()"));
    }

    #[test]
    fn test_additional_11() {
        assert!(check(r#"let x = 1 in (); x"#, "Int"));
    }

    #[test]
    fn test_additional_12() {
        assert!(check(r#"let x = 1 in let y = () in (); x + 1"#, "Int"));
    }

    #[test]
    fn test_additional_13() {
        assert!(check(r#"let x = () in (); let y = () in x"#, "()"));
    }

    #[test]
    fn test_additional_14() {
        assert!(check(
            r#"\f.\g.\x. f (g x)"#,
            "(a -> b) -> (c -> a) -> c -> b"
        ));
    }

    #[test]
    fn test_additional_15() {
        assert!(check(
            r#"\f.\x.f (f (f (f (f (f x)))))"#,
            "(a -> a) -> a -> a"
        ));
    }

    #[test]
    fn test_additional_16() {
        assert!(check(
            r#"\f.\g.\x. f (g (g x))"#,
            "(a -> b) -> (a -> a) -> a -> b"
        ));
    }
    // Add similar individual test functions for the remaining test cases
}
