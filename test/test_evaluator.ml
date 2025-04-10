open Alcotest
open Sand_types.Token
open Sand_evaluator.Evaluator

(* let equal_token a b = a.token = b.token && a.offset = b.offset *)
(* let pp_token fmt tk = Format.fprintf fmt "{ token = %s; offset: %d } " (Sand_types.Token.to_string tk.token) tk.offset *)
(* let test_token = testable pp_token equal_token *)

let big_int_bool = Alcotest.(pair (of_pp Z.pp_print) bool)

let string_int = pair string int

let result_token_list = result big_int_bool string_int

let empty () =
    let expected = Error ("Empty input", 0) in
    let actual = eval [] None in
    check result_token_list "empty" expected actual

let no_previous_ans () =
    let expected = Error ("There is no previous ans", 2) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Ans; offset = 2};
        {token = Addition; offset = 1};
    ] None in
    check result_token_list "no previous ans" expected actual

let previous_ans () =
    let expected = Ok (Z.of_string "10", false) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Ans; offset = 2};
        {token = Addition; offset = 1};
    ] (Some (Z.of_string "5")) in
    check result_token_list "previous ans" expected actual

let too_many_operands () =
    let expected = Error ("Too many operands", 0) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "5"); offset = 1};
        {token = Number (Z.of_string "5"); offset = 3};
        {token = Addition; offset = 2};
    ] None in
    check result_token_list "too many operands" expected actual

let missing_operand_unary () =
    let expected = Error ("Missing operand", 0) in
    let actual = eval [
        {token = BitwiseComplement; offset = 0};
    ] None in
    check result_token_list "missing operand unary" expected actual

let two_missing_operand_binary () =
    let expected = Error ("Missing operand", 0) in
    let actual = eval [
        {token = FloorDivision; offset = 0};
    ] None in
    check result_token_list "two missing operand binary" expected actual

let one_missing_operand_binary () =
    let expected = Error ("Missing operand", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = FloorDivision; offset = 1};
    ] None in
    check result_token_list "one missing operand binary" expected actual

let div_by_zero () =
    let expected = Error ("Division by zero", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "0"); offset = 2};
        {token = FloorDivision; offset = 1};
    ] None in
    check result_token_list "division by zero" expected actual

let mod_by_zero () =
    let expected = Error ("Mod by zero", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "0"); offset = 2};
        {token = Modulo; offset = 1};
    ] None in
    check result_token_list "mod by zero" expected actual

let negative_exponent () =
    let expected = Error ("Pow cannot be negative", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "-1"); offset = 2};
        {token = Pow; offset = 1};
    ] None in
    check result_token_list "negative exponent" expected actual

let exponent_doesnt_fit_int () =
    let expected = Error ("Pow must fit into int63", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "0xFFFFFFFFFFFFFFFF"); offset = 2};
        {token = Pow; offset = 1};
    ] None in
    check result_token_list "exponent that does not find int63" expected actual

let left_shift_by_neg () =
    let expected = Error ("Right operand when shifting cannot be less than zero", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "-1"); offset = 2};
        {token = ShiftLeft; offset = 1};
    ] None in
    check result_token_list "left shift by neg" expected actual

let left_shift_by_too_large () =
    let expected = Error ("Right operand when shifting must fit into int63", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "0xFFFFFFFFFFFFFFFF"); offset = 2};
        {token = ShiftLeft; offset = 1};
    ] None in
    check result_token_list "left shift by too large" expected actual

let right_shift_by_neg () =
    let expected = Error ("Right operand when shifting cannot be less than zero", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "-1"); offset = 2};
        {token = ShiftRight; offset = 1};
    ] None in
    check result_token_list "right shift by neg" expected actual

let right_shift_by_too_large () =
    let expected = Error ("Right operand when shifting must fit into int63", 1) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "0xFFFFFFFFFFFFFFFF"); offset = 2};
        {token = ShiftRight; offset = 1};
    ] None in
    check result_token_list "right shift by too large" expected actual

let multiple_unary_ops () =
    let expected = Ok (Z.of_string "4", false) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Negative; offset = 1};
        {token = BitwiseComplement; offset = 1};
    ] None in
    check result_token_list "right shift by too large" expected actual

let complex_expression_one () =
    let expected = Ok (Z.of_string "256", false) in
    let actual = eval [
        {token = Number (Z.of_string "5"); offset = 0};
        {token = Number (Z.of_string "3"); offset = 1};
        {token = Addition; offset = 2};
        {token = Number (Z.of_string "2"); offset = 3};
        {token = Number (Z.of_string "7"); offset = 4};
        {token = Number (Z.of_string "2"); offset = 5};
        {token = Subtraction; offset = 6};
        {token = Pow; offset = 7};
        {token = Multiplication; offset = 8};
    ] None in
    check result_token_list "complex expression one" expected actual

let complex_expression_two () =
    let expected = Ok (Z.of_string "9", false) in
    let actual = eval [
        {token = Number (Z.of_string "7"); offset = 0};
        {token = Number (Z.of_string "2"); offset = 1};
        {token = Number (Z.of_string "3"); offset = 2};
        {token = Multiplication; offset = 3};
        {token = Addition; offset = 4};
        {token = Number (Z.of_string "4"); offset = 5};
        {token = BitwiseComplement; offset = 6};
        {token = BitwiseAND; offset = 7};
    ] None in
    check result_token_list "complex expression two" expected actual

let complex_expression_three () =
    let expected = Ok (Z.of_string "1", false) in
    let actual = eval [
        {token = Number (Z.of_string "10"); offset = 0};
        {token = Number (Z.of_string "3"); offset = 1};
        {token = ShiftLeft; offset = 2};
        {token = Number (Z.of_string "5"); offset = 3};
        {token = Number (Z.of_string "2"); offset = 4};
        {token = ShiftRight; offset = 5};
        {token = BitwiseOR; offset = 6};
        {token = Number (Z.of_string "7"); offset = 7};
        {token = BitwiseAND; offset = 8};
        {token = Number (Z.of_string "9"); offset = 9};
        {token = Pow; offset = 10};
    ] None in
    check result_token_list "complex expression three" expected actual

let complex_expression_four () =
    let expected = Ok (Z.of_string "3", false) in
    let actual = eval [
        {token = Number (Z.of_string "12"); offset = 0};
        {token = Number (Z.of_string "8"); offset = 1};
        {token = BitwiseOR; offset = 2};
        {token = Number (Z.of_string "4"); offset = 3};
        {token = BitwiseAND; offset = 4};
        {token = BitwiseComplement; offset = 5};
        {token = Number (Z.of_string "5"); offset = 6};
        {token = Addition; offset = 7};
        {token = Number (Z.of_string "2"); offset = 8};
        {token = Multiplication; offset = 9};
        {token = Number (Z.of_string "6"); offset = 10};
        {token = Modulo; offset = 11};
        {token = Number (Z.of_string "3"); offset = 12};
        {token = Addition; offset = 13};
    ] None in
    check result_token_list "complex expression four" expected actual

let suite = [
    "Empty", `Quick, empty;
    "No previous answer", `Quick, no_previous_ans;
    "Previous answer", `Quick, previous_ans;
    "Too many operands", `Quick, too_many_operands;
    "Missing operand unary", `Quick, missing_operand_unary;
    "One operand missing binary", `Quick, one_missing_operand_binary;
    "Two operand missing binary", `Quick, two_missing_operand_binary;
    "Division by zero", `Quick, div_by_zero;
    "Mod by zero", `Quick, mod_by_zero;
    "Negative exponent", `Quick, negative_exponent;
    "Pow that doesnt fit", `Quick, exponent_doesnt_fit_int;
    "Left shift by neg", `Quick, left_shift_by_neg;
    "Left shift by too large", `Quick, left_shift_by_too_large;
    "Right shift by neg", `Quick, right_shift_by_neg;
    "Right shift by too large", `Quick, right_shift_by_too_large;
    "Multiple unary operations", `Quick, multiple_unary_ops;
    "Complex expression (1)", `Quick, complex_expression_one;
    "Complex expression (2)", `Quick, complex_expression_two;
    "Complex expression (3)", `Quick, complex_expression_three;
    "Complex expression (4)", `Quick, complex_expression_four;
]

let () = Alcotest.run "Evaluator Tests" [ ("basic", suite )]
