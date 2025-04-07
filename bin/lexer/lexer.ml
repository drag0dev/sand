type token = Sand_types.Token.t;;

let parse_hex_num input =
    let rec find_len input len acc =
        match input with
        | [] -> ([], len, List.rev acc)
        | c :: t when (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') -> find_len t (len+1) (c :: acc)
        | l -> (l, len, List.rev acc) in
    let (l, len, num) = find_len input 0 [] in
    let num = '0' :: 'x' :: num in
    match len == 0 with
    | true -> Error "Missing a digit"
    | false ->
        let len = len + 2 in
        let num = num |> List.to_seq |> String.of_seq in
        let num = Z.of_substring num ~pos:0 ~len:len in
        Ok (l, len, Sand_types.Token.Number num);;

let parse_oct_num input =
    let rec find_len input len acc =
        match input with
        | [] -> ([], len, List.rev acc)
        | c :: t when c >= '0' && c <= '7' -> find_len t (len+1) (c :: acc)
        | l -> (l, len, List.rev acc) in
    let (l, len, num) = find_len input 0 [] in
    let num = '0' :: 'o' :: num in
    match len == 0 with
    | true -> Error "Missing a digit"
    | false ->
        let len = len + 2 in
        let num = num |> List.to_seq |> String.of_seq in
        let num = Z.of_substring num ~pos:0 ~len:len in
        Ok (l, len, Sand_types.Token.Number num);;

let parse_bin_num input =
    let rec find_len input len acc =
        match input with
        | [] -> ([], len, List.rev acc)
        | c :: t when c >= '0' && c <= '1' -> find_len t (len+1) (c :: acc)
        | l -> (l, len, List.rev acc) in
    let (l, len, num) = find_len input 0 [] in
    let num = '0' :: 'b' :: num in
    match len == 0 with
    | true -> Error "Missing a digit"
    | false ->
        let len = len + 2 in
        let num = num |> List.to_seq |> String.of_seq in
        let num = Z.of_substring num ~pos:0 ~len:len in
        Ok (l, len, Sand_types.Token.Number num);;

let parse_dec_num input =
    let rec find_len input len acc =
        match input with
        | [] -> ([], len, List.rev acc)
        | c :: t when c >= '0' && c <='9' -> find_len t (len+1) (c :: acc)
        | l -> (l, len, List.rev acc) in
    let (l, len, num) = find_len input 0 [] in
    let num = num |> List.to_seq |> String.of_seq in
    let num = Z.of_substring num ~pos:0 ~len:len in
    (l, len, Sand_types.Token.Number num);;

let rec tokenize_aux input pos acc =
    match input with
        | [] -> Ok (List.rev acc)
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
                (match parse_hex_num t with
                | Error msg -> Error (msg, pos+1)
                | Ok (input, move, tk) ->
                    let acc = tk :: acc in
                    tokenize_aux input (pos+move) acc)
        | '0' :: 'o' :: t ->
                (match parse_oct_num t with
                | Error msg -> Error (msg, pos+1)
                | Ok (input, move, tk) ->
                    let acc = tk :: acc in
                    tokenize_aux input (pos+move) acc)
        | '0' :: 'b' :: t ->
                (match parse_bin_num t with
                | Error msg -> Error (msg, pos+1)
                | Ok (input, move, tk) ->
                    let acc = tk :: acc in
                    tokenize_aux input (pos+move) acc)
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
                let (input, move, tk) = parse_dec_num (c::t) in
                let acc = tk :: acc in
                tokenize_aux input (pos+move) acc
        | _ -> Error ("Unknown symbol", pos)

let tokenize (input : string) : (token list, string * int) result =
    let input = input |> String.lowercase_ascii |> String.to_seq |> List.of_seq in
    tokenize_aux input 0 [];;
