open Sand_types.Token
type token = Sand_types.Token.located_token

let precedence = function
    | Addition -> 1
    | Subtraction -> 1
    | Multiplication -> 2
    | FloorDivision -> 2
    | Modulo -> 2
    | Pow -> 3
    | BitwiseAND -> 1
    | BitwiseOR -> 1
    | BitwiseXOR -> 1
    | ShiftLeft -> 1
    | ShiftRight -> 1
    | BitwiseNAND -> 1
    | BitwiseNOR -> 1
    | BitwiseXNOR -> 1
    | BitwiseComplement -> 5
    | OpeningParenthesis -> 4
    | ClosingParenthesis -> 4
    | Positive -> 5
    | Negative -> 5
    | _ -> failwith "ans/exit/number are not operators";;

type previous_token =
    | None
    | LeftParen
    | RightParen
    | Operator
    | Operand;;

let rec drain_till_opening_paren op_stack output_q found_left_paren =
    match op_stack with
    | [] -> (op_stack, output_q, found_left_paren)
    | tk :: t ->
            let {token; _} = tk in
            match token with
            | OpeningParenthesis -> (t, output_q, true)
            | _ ->
                    let output_q = tk :: output_q in
                    drain_till_opening_paren t output_q found_left_paren;;

let rec drain_remaining op_stack output_q =
    match op_stack with
    | [] -> Ok output_q
    | tk :: t ->
            let {token; offset} = tk in
            match token with
            | OpeningParenthesis -> Error ("Unmatched left parenthesis", offset)
            | _ ->
                    let output_q = tk :: output_q in
                    drain_remaining t output_q;;

let rec apply_precedence op_stack output_q curr_precedence =
    match op_stack with
    | [] -> (op_stack, output_q)
    | tk :: t ->
            let {token; _} = tk in
            match token with
            | OpeningParenthesis -> (op_stack, output_q)
            | ClosingParenthesis -> (op_stack, output_q)
            | Positive | Negative | BitwiseComplement ->
                    if precedence token <= curr_precedence
                    then
                        (op_stack, output_q)
                    else
                        let output_q = tk :: output_q in
                        apply_precedence t output_q curr_precedence
            | op ->
                    if precedence op < curr_precedence
                    then
                        (op_stack, output_q)
                    else
                        let output_q = tk :: output_q in
                        apply_precedence t output_q curr_precedence;;

let rec shunting_yard input prev_tk output_q op_stack =
    match input with
    | [] ->
            if prev_tk = Operator
            then
                Error ("Expression cannot end with an operator", (-1))
            else
                (match drain_remaining op_stack output_q with
                | Error e -> Error e
                | Ok output_q -> Ok (List.rev output_q, false))
    | tk :: t ->
            let {token; offset} = tk in
            match token with
            | Exit ->
                if prev_tk = Operator
                then
                    Error ("Expression cannot end with an operator", (-1))
                else
                    (match drain_remaining op_stack output_q with
                    | Error e -> Error e
                    | Ok output_q -> Ok (List.rev output_q, true))
            | Number _ ->
                    if prev_tk = Operand || prev_tk = RightParen
                    then
                        Error ("Missing operator", offset)
                    else
                        let output_q = tk :: output_q in
                        shunting_yard t Operand output_q op_stack
            | Ans ->
                    if prev_tk = Operand || prev_tk = RightParen
                    then
                        Error ("Missing operator", offset)
                    else
                        let output_q = tk :: output_q in
                        shunting_yard t Operand output_q op_stack
            | OpeningParenthesis ->
                    if prev_tk = Operand
                    then
                        Error ("Missing operator before parenthesis", offset)
                    else
                        let op_stack = tk :: op_stack in
                        shunting_yard t LeftParen output_q op_stack
            | ClosingParenthesis ->
                    if prev_tk = Operator || prev_tk = None
                    then
                        Error("Invalid parenthesis", offset)
                    else
                        let (op_stack, output_q, found_left_paren) = drain_till_opening_paren op_stack output_q false in
                        if not found_left_paren
                        then
                            Error("Unmatched right parenthesis", offset)
                        else
                            shunting_yard t RightParen output_q op_stack
            | op ->
                    let op = if (token = Addition || token = Subtraction) &&
                                (prev_tk == Operator || prev_tk == None || prev_tk == LeftParen)
                    then (if token = Addition then Positive else Negative)
                    else op in

                    let (op_stack, output_q) = apply_precedence op_stack output_q (precedence op) in
                    let tk = {token = op; offset = offset} in
                    let op_stack = tk :: op_stack in
                    shunting_yard t Operator output_q op_stack;;

let infix_to_postfix (input : token list) : (token list * bool, string * int) result =
    shunting_yard input None [] [];;
