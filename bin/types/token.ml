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


let to_string = function
    | Number n -> "NUMBER(" ^ string_of_int n ^ ")"
    | Addition -> "ADDITION"
    | Subtraction -> "SUBTRACTION"
    | Multiplication -> "MULTIPLICATION"
    | FloorDivision -> "FLOOR_DIVISION"
    | Modulo -> "MODULO"
    | Exponent -> "EXPONENT"
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
    | Ans -> "ANS"
    | Exit -> "EXIT"

let equal t1 t2 = t1 = t2

type located_token = {
    token: t;
    offset: int;
}
