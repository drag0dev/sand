type token = Sand_types.Token.located_token

val infix_to_postfix : token list -> (token list, string * int) result
