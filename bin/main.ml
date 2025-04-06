open Sand_lexer;;

Printf.printf "> ";
let input = read_line () in
let _ = Lexer.tokenize input in
Printf.printf "done\n";;
