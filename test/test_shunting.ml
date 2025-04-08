open Alcotest
open Sand_types.Token
open Sand_evaluator.Shunting_yard

let equal_token a b = a.token = b.token && a.offset = b.offset

let pp_token fmt tk = Format.fprintf fmt "{ token = %s; offset: %d } " (Sand_types.Token.to_string tk.token) tk.offset

let test_token = testable pp_token equal_token

let token_list = list test_token

let string_int = pair string int

let result_token_list = result token_list string_int

let empty () =
    let expected = Ok [] in
    let actual = infix_to_postfix [] in
    check result_token_list "empty" expected actual

let one_unary () =
    let num = Z.of_string "0x10" in
    let input = [
        {token = BitwiseComplement; offset = 0};
        {token = Number num; offset = 1};
    ] in
    let expected = Ok [
        {token = Number num; offset = 1};
        {token = BitwiseComplement; offset = 0};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "one unary" expected actual

let simple_precedence_one () =
    let input = [
        {token = Number (Z.of_string "3"); offset = 0};
        {token = Addition; offset = 1};
        {token = Number (Z.of_string "4"); offset = 2};
        {token = Multiplication; offset = 3};
        {token = Number (Z.of_string "2"); offset = 4};
    ] in
    let expected = Ok [
        {token = Number (Z.of_string "3"); offset = 0};
        {token = Number (Z.of_string "4"); offset = 2};
        {token = Number (Z.of_string "2"); offset = 4};
        {token = Multiplication; offset = 3};
        {token = Addition; offset = 1};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "simple precedence one" expected actual

let simple_precedence_two () =
    let input = [
        {token = Number (Z.of_string "3"); offset = 0};
        {token = Multiplication; offset = 1};
        {token = Number (Z.of_string "4"); offset = 2};
        {token = Addition; offset = 3};
        {token = Number (Z.of_string "2"); offset = 4};
    ] in
    let expected = Ok [
        {token = Number (Z.of_string "3"); offset = 0};
        {token = Number (Z.of_string "4"); offset = 2};
        {token = Multiplication; offset = 1};
        {token = Number (Z.of_string "2"); offset = 4};
        {token = Addition; offset = 3};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "simple precedence two" expected actual

let simple_precedence_three () =
    let input = [
        {token = Number (Z.of_string "3"); offset = 0};
        {token = BitwiseAND; offset = 1};
        {token = Number (Z.of_string "4"); offset = 2};
        {token = BitwiseOR; offset = 3};
        {token = Number (Z.of_string "2"); offset = 4};
    ] in
    let expected = Ok [
        {token = Number (Z.of_string "3"); offset = 0};
        {token = Number (Z.of_string "4"); offset = 2};
        {token = BitwiseAND; offset = 1};
        {token = Number (Z.of_string "2"); offset = 4};
        {token = BitwiseOR; offset = 3};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "simple precedence three" expected actual

let simple_precedence_four () =
    let input = [
        {token = BitwiseComplement; offset = 0};
        {token = Number (Z.of_string "3"); offset = 1};
        {token = Addition; offset = 2};
        {token = Number (Z.of_string "2"); offset = 3};
    ] in
    let expected = Ok [
        {token = Number (Z.of_string "3"); offset = 1};
        {token = BitwiseComplement; offset = 0};
        {token = Number (Z.of_string "2"); offset = 3};
        {token = Addition; offset = 2};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "simple precedence four" expected actual

let simple_precedence_five () =
    let input = [
        {token = BitwiseComplement; offset = 0};
        {token = Negative; offset = 1};
        {token = Number (Z.of_string "3"); offset = 2};
    ] in
    let expected = Ok [
        {token = Number (Z.of_string "3"); offset = 2};
        {token = Negative; offset = 1};
        {token = BitwiseComplement; offset = 0};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "simple precedence five" expected actual

let simple_parens () =
    let input = [
        {token = Number (Z.of_string "2"); offset = 0};
        {token = Multiplication; offset = 1};
        {token = OpeningParenthesis; offset = 2};
        {token = Number (Z.of_string "3"); offset = 3};
        {token = Multiplication; offset = 4};
        {token = OpeningParenthesis; offset = 5};
        {token = Number (Z.of_string "4"); offset = 6};
        {token = Subtraction; offset = 7};
        {token = Number (Z.of_string "1"); offset = 8};
        {token = ClosingParenthesis; offset = 9};
        {token = ClosingParenthesis; offset = 10};

    ] in
    let expected = Ok [
        {token = Number (Z.of_string "2"); offset = 0};
        {token = Number (Z.of_string "3"); offset = 3};
        {token = Number (Z.of_string "4"); offset = 6};
        {token = Number (Z.of_string "1"); offset = 8};
        {token = Subtraction; offset = 7};
        {token = Multiplication; offset = 4};
        {token = Multiplication; offset = 1};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "simple paren" expected actual

let complex_precedence () =
    let input = [
        {token = BitwiseComplement; offset = 0};
        {token = OpeningParenthesis; offset = 1};
        {token = Number (Z.of_string "2"); offset = 2};
        {token = BitwiseAND; offset = 3};
        {token = Number (Z.of_string "3"); offset = 4};
        {token = ClosingParenthesis; offset = 5};
        {token = BitwiseOR; offset = 6};
        {token = OpeningParenthesis; offset = 7};
        {token = Number (Z.of_string "4"); offset = 8};
        {token = ShiftLeft; offset = 9};
        {token = Number (Z.of_string "1"); offset = 10};
        {token = ClosingParenthesis; offset = 11};

    ] in
    let expected = Ok [
        {token = Number (Z.of_string "2"); offset = 2};
        {token = Number (Z.of_string "3"); offset = 4};
        {token = BitwiseAND; offset = 3};
        {token = BitwiseComplement; offset = 0};
        {token = Number (Z.of_string "4"); offset = 8};
        {token = Number (Z.of_string "1"); offset = 10};
        {token = ShiftLeft; offset = 9};
        {token = BitwiseOR; offset = 6};
    ] in
    let actual = infix_to_postfix input in
    check result_token_list "complex precedence" expected actual

let unmatch_left_paren () =
    let input = [
        {token = OpeningParenthesis; offset = 0};
        {token = Number (Z.of_string "2"); offset = 1};
        {token = Multiplication; offset = 2};
        {token = Number (Z.of_string "3"); offset = 3};

    ] in
    let expected = Error ("Unmatched left parenthesis", 0) in
    let actual = infix_to_postfix input in
    check result_token_list "unmatched left parenthesis" expected actual

let unmatch_right_paren () =
    let input = [
        {token = Number (Z.of_string "2"); offset = 0};
        {token = Multiplication; offset = 1};
        {token = Number (Z.of_string "3"); offset = 2};
        {token = ClosingParenthesis; offset = 3};

    ] in
    let expected = Error ("Unmatched right parenthesis", 3) in
    let actual = infix_to_postfix input in
    check result_token_list "unmatched right parenthesis" expected actual

let expression_ends_with_op () =
    let input = [
        {token = Number (Z.of_string "2"); offset = 0};
        {token = Multiplication; offset = 1};
        {token = Number (Z.of_string "3"); offset = 2};
        {token = Multiplication; offset = 3};

    ] in
    let expected = Error ("Expression cannot end with an operator", -1) in
    let actual = infix_to_postfix input in
    check result_token_list "expression ends with an op" expected actual

let missing_op_one () =
    let input = [
        {token = OpeningParenthesis; offset = 1};
        {token = Number (Z.of_string "2"); offset = 2};
        {token = Multiplication; offset = 3};
        {token = Number (Z.of_string "3"); offset = 4};
        {token = ClosingParenthesis; offset = 5};
        {token = Number (Z.of_string "3"); offset = 6};

    ] in
    let expected = Error ("Missing operator", 6) in
    let actual = infix_to_postfix input in
    check result_token_list "missing op one" expected actual

let missing_op_two () =
    let input = [
        {token = Number (Z.of_string "2"); offset = 1};
        {token = Multiplication; offset = 2};
        {token = Number (Z.of_string "3"); offset = 3};
        {token = Number (Z.of_string "3"); offset = 4};

    ] in
    let expected = Error ("Missing operator", 4) in
    let actual = infix_to_postfix input in
    check result_token_list "missing op two" expected actual

let missing_op_three () =
    let input = [
        {token = Number (Z.of_string "2"); offset = 1};
        {token = OpeningParenthesis; offset = 2};
        {token = Number (Z.of_string "3"); offset = 3};
        {token = Multiplication; offset = 4};
        {token = Number (Z.of_string "3"); offset = 5};
        {token = ClosingParenthesis; offset = 6};

    ] in
    let expected = Error ("Missing operator before parenthesis", 2) in
    let actual = infix_to_postfix input in
    check result_token_list "missing op three" expected actual

let invalid_paren_one () =
    let input = [
        {token = Number (Z.of_string "2"); offset = 1};
        {token = OpeningParenthesis; offset = 2};
        {token = Number (Z.of_string "3"); offset = 3};
        {token = Multiplication; offset = 4};
        {token = Number (Z.of_string "3"); offset = 5};
        {token = ClosingParenthesis; offset = 6};

    ] in
    let expected = Error ("Missing operator before parenthesis", 2) in
    let actual = infix_to_postfix input in
    check result_token_list "invalid paren one" expected actual

let invalid_paren_two () =
    let input = [
        {token = ClosingParenthesis; offset = 1};
    ] in
    let expected = Error ("Invalid parenthesis", 1) in
    let actual = infix_to_postfix input in
    check result_token_list "invalid paren two" expected actual

let suite = [
    "Empty", `Quick, empty;
    "One unary", `Quick,  one_unary;
    "Simple precedence (1)", `Quick,  simple_precedence_one;
    "Simple precedence (2)", `Quick,  simple_precedence_two;
    "Simple precedence (3)", `Quick,  simple_precedence_three;
    "Simple precedence (4)", `Quick,  simple_precedence_four;
    "Simple precedence (5)", `Quick,  simple_precedence_five;
    "Simple parenthesis", `Quick,  simple_parens;
    "Complex precedence", `Quick,  complex_precedence;
    "Unmatched left parenthesis", `Quick,  unmatch_left_paren;
    "Unmatched right parenthesis", `Quick,  unmatch_right_paren;
    "Expression ends with an op", `Quick,  expression_ends_with_op;
    "Missing operator (1)", `Quick,  missing_op_one;
    "Missing operator (2)", `Quick,  missing_op_two;
    "Missing operator (3)", `Quick,  missing_op_three;
    "Invalid paren (1)", `Quick,  invalid_paren_one;
    "Invalid paren (2)", `Quick,  invalid_paren_two;
]

let () = Alcotest.run "Shunting Tests" [ ("basic", suite )]
