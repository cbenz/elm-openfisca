module Operations exposing (..)

import Interpreter exposing (..)
import Types exposing (..)


type ArithmeticOperation
    = Number Float
      -- Distinguish counts from amounts? Use ValueWithUnit?
    | Add ArithmeticOperation ArithmeticOperation
    | Negate ArithmeticOperation
    | Mul ArithmeticOperation ArithmeticOperation
    | Div ArithmeticOperation ArithmeticOperation
    | Max ArithmeticOperation ArithmeticOperation
    | Min ArithmeticOperation ArithmeticOperation
    | Condition BooleanOperation ArithmeticOperation ArithmeticOperation
    | ScaleEvaluation Scale ArithmeticOperation
    | ArithmeticError String ArithmeticOperation


type BooleanOperation
    = Boolean Bool
    | And BooleanOperation BooleanOperation
    | Or BooleanOperation BooleanOperation
    | Equals ArithmeticOperation ArithmeticOperation



-- CONSTRUCTION


add3 : ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation
add3 op1 op2 op3 =
    Add op1 op2 |> Add op3


clip : ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation
clip min max op =
    Max min (Min max op)


substract : ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation
substract op1 op2 =
    Add op1 (Negate op2)



-- EVALUATION


evalArithmeticOperation : ArithmeticOperation -> Result String Float
evalArithmeticOperation op =
    case op of
        Number float ->
            Ok float

        Add op1 op2 ->
            Result.map2
                (+)
                (evalArithmeticOperation op1)
                (evalArithmeticOperation op2)

        Negate op ->
            Result.map negate
                (evalArithmeticOperation op)

        Mul op1 op2 ->
            Result.map2
                (*)
                (evalArithmeticOperation op1)
                (evalArithmeticOperation op2)

        Div op1 op2 ->
            Result.map2
                (/)
                (evalArithmeticOperation op1)
                (evalArithmeticOperation op2)

        Max op1 op2 ->
            Result.map2
                max
                (evalArithmeticOperation op1)
                (evalArithmeticOperation op2)

        Min op1 op2 ->
            Result.map2
                min
                (evalArithmeticOperation op1)
                (evalArithmeticOperation op2)

        Condition boolOp op1 op2 ->
            -- Do not use map2 to defer evaluation in "then" or "else" branch.
            if evalBooleanOperation boolOp then
                evalArithmeticOperation op1
            else
                evalArithmeticOperation op2

        ScaleEvaluation scale op ->
            Result.map (interpretScale scale)
                (evalArithmeticOperation op)

        ArithmeticError str op ->
            Err (str ++ ": " ++ (toString (evalArithmeticOperation op)))


evalBooleanOperation : BooleanOperation -> Bool
evalBooleanOperation op =
    case op of
        Boolean bool ->
            bool

        And op1 op2 ->
            evalBooleanOperation op1 && evalBooleanOperation op2

        Or op1 op2 ->
            evalBooleanOperation op1 || evalBooleanOperation op2

        Equals op1 op2 ->
            evalArithmeticOperation op1 == evalArithmeticOperation op2
