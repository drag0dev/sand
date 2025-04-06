type token = Sand_types.Token.t;;

let parse_hex_num input =
    (input, 0, Sand_types.Token.Number 1);;

let parse_oct_num input =
    (input, 0, Sand_types.Token.Number 1);;

let parse_bin_num input =
    (input, 0, Sand_types.Token.Number 1);;

let parse_dec_num input =
    (input, 0, Sand_types.Token.Number 1);;

let rec tokenize_aux input pos acc =
    match input with
        | [] -> List.rev acc
        | ' ' :: t -> tokenize_aux t (pos+1) acc
        | 'a' :: 'n' :: 's' :: t ->
                let tk = Sand_types.Token.Ans in
                let acc = tk :: acc in
                tokenize_aux t (pos+3) acc
        | 'e' :: 'x' :: 'i' :: 't' :: t ->
                let tk = Sand_types.Token.Exit in
                let acc = tk :: acc in
                tokenize_aux t (pos+4) acc
        | '0' :: 'x' :: t ->
                let (input, move, tk) = parse_hex_num t in
                let acc = tk :: acc in
                tokenize_aux input (pos+2+move) acc
        | '0' :: 'o' :: t ->
                let (input, move, tk) = parse_oct_num t in
                let acc = tk :: acc in
                tokenize_aux input (pos+2+move) acc
        | '0' :: 'b' :: t ->
                let (input, move, tk) = parse_bin_num t in
                let acc = tk :: acc in
                tokenize_aux input (pos+2+move) acc
        | '<' :: '<' :: t ->
                let tk = Sand_types.Token.ShiftLeft in
                let acc = tk :: acc in
                tokenize_aux t (pos+2) acc
        | '>' :: '>' :: t ->
                let tk = Sand_types.Token.ShiftRight in
                let acc = tk :: acc in
                tokenize_aux t (pos+2) acc
        | '*' :: '*' :: t ->
                let tk = Sand_types.Token.Exponent in
                let acc = tk :: acc in
                tokenize_aux t (pos+2) acc
        | '!' :: '&' :: t ->
                let tk = Sand_types.Token.BitwiseNAND in
                let acc = tk :: acc in
                tokenize_aux t (pos+2) acc
        | '!' :: '|' :: t ->
                let tk = Sand_types.Token.BitwiseNOR in
                let acc = tk :: acc in
                tokenize_aux t (pos+2) acc
        | '!' :: '^' :: t ->
                let tk = Sand_types.Token.BitwiseXNOR in
                let acc = tk :: acc in
                tokenize_aux t (pos+2) acc
        | '+' :: t ->
                let tk = Sand_types.Token.Addition in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '-' :: t ->
                let tk = Sand_types.Token.Subtraction in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '*' :: t ->
                let tk = Sand_types.Token.Multiplication in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '/' :: t ->
                let tk = Sand_types.Token.FloorDivision in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '%' :: t ->
                let tk = Sand_types.Token.Modulo in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '&' :: t ->
                let tk = Sand_types.Token.BitwiseAND in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '|' :: t ->
                let tk = Sand_types.Token.BitwiseOR in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '^' :: t ->
                let tk = Sand_types.Token.BitwiseXOR in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '~' :: t ->
                let tk = Sand_types.Token.BitwiseComplement in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | '(' :: t ->
                let tk = Sand_types.Token.OpeningParenthesis in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | ')' :: t ->
                let tk = Sand_types.Token.ClosingParenthesis in
                let acc = tk :: acc in
                tokenize_aux t (pos+1) acc
        | c :: t when c >= '0' && c <= '9' ->
                let (input, move, tk) = parse_dec_num t in
                let acc = tk :: acc in
                tokenize_aux input (pos+move) acc
        | _ -> assert false;;

let tokenize (input : string) : token list =
    let input = input |> String.lowercase_ascii |> String.to_seq |> List.of_seq in
    tokenize_aux input 0 [];;
