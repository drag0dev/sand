type t =
    | Number of int

    (* binary operators *)
    | Addition
    | Subtraction
    | Multiplication
    | FloorDivision
    | Modulo
    | Exponent
    | BitwiseAND
    | BitwiseOR
    | BitwiseXOR
    | ShiftLeft
    | ShiftRight
    | BitwiseNAND
    | BitwiseNOR
    | BitwiseXNOR

    (* unary operators *)
    | BitwiseComplement (* NOT *)

    (* other operators *)
    | OpeningParenthesis
    | ClosingParenthesis

    (* reserver words *)
    | Ans (* result of the pervious calculation *)
    | Exit

val to_string : t -> string
val equal : t -> t -> bool

type located_token = {
    token: t;
    offset: int;
}
