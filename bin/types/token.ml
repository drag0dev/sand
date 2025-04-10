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


let to_string = function
    | Number n -> "NUMBER(" ^ Z.to_string n ^ ")"
    | Addition -> "ADDITION"
    | Subtraction -> "SUBTRACTION"
    | Multiplication -> "MULTIPLICATION"
    | FloorDivision -> "FLOOR_DIVISION"
    | Modulo -> "MODULO"
    | Pow -> "Pow"
    | BitwiseAND -> "BITWISE_AND"
    | BitwiseOR -> "BITWISE_OR"
    | BitwiseXOR -> "BITWISE_XOR"
    | ShiftLeft -> "SHIFT_LEFT"
    | ShiftRight -> "SHIFT_RIGHT"
    | BitwiseNAND -> "BITWISE_NAND"
    | BitwiseNOR -> "BITWISE_NOR"
    | BitwiseXNOR -> "BITWISE_XNOR"
    | BitwiseComplement -> "NOT"
    | OpeningParenthesis -> "("
    | ClosingParenthesis -> ")"
    | Positive -> "POSITIVE"
    | Negative -> "NEGATIVE"
    | Ans -> "ANS"
    | Exit -> "EXIT"

let equal t1 t2 = t1 = t2

type located_token = {
    token: t;
    offset: int;
}
