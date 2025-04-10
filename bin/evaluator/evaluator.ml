open Sand_types.Token
type token = Sand_types.Token.located_token

let rec aux_eval input prev_ans exit_found stack =
    match input with
    | [] -> Ok (stack, exit_found)
    | tk :: t ->
            let {token; offset} = tk in
            match token with
            | Exit -> Ok (stack, true)
            | Number n ->
                    let stack = n :: stack in
                    aux_eval t prev_ans false stack
            | Ans ->
                    if Option.is_none prev_ans
                    then Error ("There is no previous ans", offset)
                    else
                        let num = Option.get prev_ans in
                        let stack = num :: stack in
                        aux_eval t prev_ans false stack
            | Positive ->
                    (match stack with
                    | [] -> Error ("Missing operand", offset)
                    | operand :: operand_t ->
                            let operand = Z.abs operand in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack)
            | Negative ->
                    (match stack with
                    | [] -> Error ("Missing operand", offset)
                    | operand :: operand_t ->
                            let operand = Z.neg operand in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack)
            | BitwiseComplement ->
                    (match stack with
                    | [] -> Error ("Missing operand", offset)
                    | operand :: operand_t ->
                            let operand = Z.lognot operand in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack)
            | Addition ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.add left right in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | Subtraction ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.sub left right in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | Multiplication ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.mul left right in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | FloorDivision ->
                    (match stack with
                    | right :: left :: operand_t ->
                            if Z.equal right (Z.of_string "0")
                            then Error ("Division by zero", offset)
                            else
                                let operand = Z.div left right in
                                let stack = operand :: operand_t in
                                aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | Modulo ->
                    (match stack with
                    | right :: left :: operand_t ->
                            if Z.equal right (Z.of_string "0")
                            then Error ("Mod by zero", offset)
                            else
                                let operand = Z.(mod) left right in
                                let stack = operand :: operand_t in
                                aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | Pow ->
                    (match stack with
                    | right :: left :: operand_t ->
                            if not (Z.fits_int right)
                            then Error("Exponent must fit into int63", offset)
                            else
                                if Z.lt right (Z.of_string "0")
                                then Error("Exponent cannot be negative", offset)
                                else
                                    let exp = Z.to_int right in
                                    let operand = Z.pow left exp in
                                    let stack = operand :: operand_t in
                                    aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | ShiftLeft ->
                    (match stack with
                    | right :: left :: operand_t ->
                            if Z.lt right (Z.of_string "0")
                            then Error ("Right operand when shifting cannot be less than zero", offset)
                            else
                                if not (Z.fits_int right)
                                then Error ("Right operand when shifting must fit into int63", offset)
                                else
                                    let right = Z.to_int right in
                                    let operand = Z.(lsl) left right in
                                    let stack = operand :: operand_t in
                                    aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | ShiftRight ->
                    (match stack with
                    | right :: left :: operand_t ->
                            if Z.lt right (Z.of_string "0")
                            then Error ("Right operand when shifting cannot be less than zero", offset)
                            else
                                if not (Z.fits_int right)
                                then Error ("Right operand when shifting must fit into int63", offset)
                                else
                                    let right = Z.to_int right in
                                    let operand = Z.(asr) left right in
                                    let stack = operand :: operand_t in
                                    aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | BitwiseAND ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.logand left right in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | BitwiseOR ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.logor left right in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | BitwiseXOR ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.logxor left right in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | BitwiseNAND ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.logand left right in
                            let operand = Z.lognot operand in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | BitwiseNOR ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.logor left right in
                            let operand = Z.lognot operand in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | BitwiseXNOR ->
                    (match stack with
                    | right :: left :: operand_t ->
                            let operand = Z.logxor left right in
                            let operand = Z.lognot operand in
                            let stack = operand :: operand_t in
                            aux_eval t prev_ans false stack
                    | _ -> Error ("Missing operand", offset))
            | _ -> failwith "Parenthesis cannot make it to the evaluator"


let eval (input: token list) (ans : Z.t option) : (Z.t * bool, string * int) result =
    match aux_eval input ans false [] with
    | Error e -> Error e
    | Ok res ->
            let (stack, exit) = res in
            match List.length stack with
            | 0 -> Error ("Empty input", 0)
            | 1 -> Ok (List.hd stack, exit)
            | _ -> Error ("Too many operands", 0);;
