open Alcotest
open Sand_types.Token

let equal_token a b = a.token = b.token && a.offset = b.offset

let pp_token fmt tk = Format.fprintf fmt "{ token = %s; offset: %d } " (Sand_types.Token.to_string tk.token) tk.offset

let test_token = testable pp_token equal_token

let token_list = list test_token

let string_int = pair string int

let result_token_list = result token_list string_int

let test_empty_input () =
    let expected = Ok [] in
    let actual = Sand_lexer.Lexer.tokenize "" in
    check result_token_list "empty input" expected actual

let test_just_spaces () =
    let expected = Ok [] in
    let actual = Sand_lexer.Lexer.tokenize "                                " in
    check result_token_list "just spaces" expected actual

let test_spaces () =
    let tk = {token = Ans; offset = 12} in
    let expected = Ok [tk] in
    let actual = Sand_lexer.Lexer.tokenize "            ans                    " in
    check result_token_list "spaces with ans" expected actual

let reserved_words () =
    let tokens = [{token = Ans; offset = 0}; {token = Exit; offset = 4}] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "ans exit" in
    check result_token_list "reserved words" expected actual

let hex_number_no_digit () =
    let expected = Error ("Missing a digit", 1) in
    let actual = Sand_lexer.Lexer.tokenize "0x" in
    check result_token_list "hex missing a digit" expected actual

let octal_number_no_digit () =
    let expected = Error ("Missing a digit", 1) in
    let actual = Sand_lexer.Lexer.tokenize "0o" in
    check result_token_list "octal missing a digit" expected actual

let binary_number_no_digit () =
    let expected = Error ("Missing a digit", 1) in
    let actual = Sand_lexer.Lexer.tokenize "0b" in
    check result_token_list "binary missing a digit" expected actual

let valid_numbers () =
    let tokens = [
        {token = Number(Z.of_string "0x10"); offset = 0};
        {token = Number(Z.of_string "0o10"); offset = 5};
        {token = Number(Z.of_string "0b10"); offset = 10};
        {token = Number(Z.of_string "10"); offset = 15};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "0x10 0o10 0b10 10" in
    check result_token_list "valid numbers" expected actual

let valid_numbers_no_spaces () =
    let tokens = [
        {token = Number(Z.of_string "0x10"); offset = 0};
        {token = Number(Z.of_string "0o10"); offset = 4};
        {token = Number(Z.of_string "0b10"); offset = 8};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "0x100o100b10" in
    check result_token_list "valid number no spaces" expected actual

let two_char_ops () =
    let tokens = [
        {token = ShiftLeft; offset = 0};
        {token = ShiftRight; offset = 2};
        {token = Pow; offset = 4};
        {token = BitwiseNAND; offset = 6};
        {token = BitwiseNOR; offset = 8};
        {token = BitwiseXNOR; offset = 10};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "<<>>**!&!|!^" in
    check result_token_list "two char ops" expected actual

let single_char_ops () =
    let tokens = [
        {token = Addition; offset = 0};
        {token = Subtraction; offset = 1};
        {token = Multiplication; offset = 2};
        {token = FloorDivision; offset = 3};
        {token = Modulo; offset = 4};
        {token = BitwiseAND; offset = 5};
        {token = BitwiseOR; offset = 6};
        {token = BitwiseXOR; offset = 7};
        {token = BitwiseComplement; offset = 8};
        {token = OpeningParenthesis; offset = 9};
        {token = ClosingParenthesis; offset = 10};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "+-*/%&|^~()" in
    check result_token_list "single char ops" expected actual

let mod_and_mult () =
    let tokens = [
        {token = Pow; offset = 0};
        {token = Pow; offset = 2};
        {token = Multiplication; offset = 4};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "*****" in
    check result_token_list "mod and mult" expected actual

let expression_no_spaces () =
    let tokens = [
        {token = Number(Z.of_string "0x10"); offset = 0};
        {token = Pow; offset = 4};
        {token = Number(Z.of_string "0b1010"); offset = 6};
        {token = Multiplication; offset = 12};
        {token = Ans; offset = 13};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "0x10**0b1010*ans" in
    check result_token_list "expression no spaces" expected actual

let expression_with_spaces () =
    let tokens = [
        {token = Number(Z.of_string "0x10"); offset = 0};
        {token = Pow; offset = 5};
        {token = Number(Z.of_string "0b1010"); offset = 8};
        {token = Multiplication; offset = 15};
        {token = Ans; offset = 17};
    ] in
    let expected = Ok tokens in
    let actual = Sand_lexer.Lexer.tokenize "0x10 ** 0b1010 * ans" in
    check result_token_list "expression with spaces" expected actual

let unknown_symbol () =
    let expected = Error ("Unknown symbol", 4) in
    let actual = Sand_lexer.Lexer.tokenize "0x10k + 0b1010" in
    check result_token_list "unknown symbol" expected actual

let suite = [
    "Empty input", `Quick, test_empty_input;
    "Just spaces", `Quick, test_just_spaces;
    "Spaces", `Quick, test_spaces;
    "Reserved words", `Quick, reserved_words;
    "Hex no digit", `Quick, hex_number_no_digit;
    "Octal no digit", `Quick, octal_number_no_digit;
    "Binary no digit", `Quick, binary_number_no_digit;
    "Valid numbers", `Quick, valid_numbers;
    "Valid numbers no space", `Quick, valid_numbers_no_spaces;
    "Two char ops", `Quick, two_char_ops;
    "Single char ops", `Quick, single_char_ops;
    "Mod and mult", `Quick, mod_and_mult;
    "Expression no spaces", `Quick, expression_no_spaces;
    "Expression with spaces", `Quick, expression_with_spaces;
    "Unknown symbol", `Quick, unknown_symbol;
]

let () = Alcotest.run "Lexer Tests" [ ("basic", suite )]
