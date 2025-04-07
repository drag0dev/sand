type token = Sand_types.Token.located_token

val tokenize : string -> (token list, string * int) result
