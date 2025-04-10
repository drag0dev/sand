type token = Sand_types.Token.located_token

val eval : token list -> Z.t option -> (Z.t * bool, string * int) result
