type token = Sand_types.Token.located_token

val infix_to_postfix : token list -> (token list * bool, string * int) result
