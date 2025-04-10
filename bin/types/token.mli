type t =
    | Number of Z.t

    (* binary operators *)
    | Addition
    | Subtraction
    | Multiplication
    | FloorDivision
    | Modulo
    | Pow
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
    | Positive
    | Negative

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
