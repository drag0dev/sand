open Sand_lexer;;
open Sand_types.Token;;
open Sand_evaluator.Shunting_yard;;
open Sand_evaluator.Evaluator;;

let is_exit = function
    | tk :: [] ->
            let {token = token; offset = _offset} = tk in
            token == Exit
    | _ -> false;;

let print_error msg offset input =
    let offset = if offset == -1 then (String.length input) else offset + 1 in
    let input_len = String.length input in
    let (start_idx, cursor_offset) = match offset < 20 with
        | true -> (0, offset)
        | false -> (offset-20, 20) in

    let sub_len = min 41 input_len in

    let pointer = String.make cursor_offset '-' in
    let pointer = pointer ^ "^" in

    let sub = String.sub input start_idx sub_len in
    Printf.printf "Error: %s\n" msg;
    Printf.printf "'%s'\n" sub;
    Printf.printf "%s\n" pointer;;

let print_res res =
    let dec = Z.to_string res in
    let hex = Z.format "X" res in
    let binary = Z.format "b" res in
    let octal = Z.format "o" res in
    Printf.printf "Dec: %s\n" dec;
    Printf.printf "Hex: %s\n" hex;
    Printf.printf "Binary: %s\n" binary;
    Printf.printf "Octal: %s\n\n" octal;;

let rec repl previous_ans =
    Printf.printf "(ans: ";
    if Option.is_none previous_ans
    then Printf.printf "none) "
    else Printf.printf "%s) " (Z.to_string (Option.get previous_ans));

    Printf.printf "> ";
    let input = read_line () in
    let tokens = Lexer.tokenize input in
    let (previous_ans, exit) = match tokens with
        | Error (msg, offset) ->
                print_error msg offset input;
                (previous_ans, false)
        | Ok tokens ->
                if is_exit tokens then (previous_ans, true)
                else
                (match infix_to_postfix tokens with
                    | Error (msg, offset) ->
                            print_error msg offset input;
                            (previous_ans, false)
                    | Ok (tokens, exit) ->
                            (match eval tokens previous_ans with
                                | Error (msg, offset) ->
                                        print_error msg offset input;
                                        (previous_ans, exit)
                                | Ok (res, _exit) ->
                                        print_res res;
                                        (Some res, exit))) in
    if exit then () else repl previous_ans;;

repl None;
