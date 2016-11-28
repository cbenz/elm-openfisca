module OpenFisca exposing (..)

import Types exposing (..)


type ArithmeticOperation
    = NoOp
    | Number Float
    | Add ArithmeticOperation ArithmeticOperation
    | Negate ArithmeticOperation
    | Mul ArithmeticOperation ArithmeticOperation
    | Div ArithmeticOperation ArithmeticOperation
    | Max ArithmeticOperation ArithmeticOperation
    | Min ArithmeticOperation ArithmeticOperation
    | Condition BooleanOperation ArithmeticOperation ArithmeticOperation
    | ScaleEvaluation Scale ArithmeticOperation


type BooleanOperation
    = BOTrue
    | BOFalse
    | And BooleanOperation BooleanOperation
    | Or BooleanOperation BooleanOperation
    | Equals ArithmeticOperation ArithmeticOperation


add3 : ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation
add3 op1 op2 op3 =
    Add op1 op2 |> Add op3


addN : List ArithmeticOperation -> ArithmeticOperation
addN ops =
    List.foldl Add NoOp ops


clip : ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation -> ArithmeticOperation
clip min max op =
    Max min (Min max op)