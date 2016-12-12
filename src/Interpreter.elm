module Interpreter exposing (..)

import Operations exposing (..)
import Types exposing (..)


interpretArithmeticOperation : ArithmeticOperation -> Result String Float
interpretArithmeticOperation op =
    case op of
        Number float ->
            Ok float

        Add op1 op2 ->
            Result.map2
                (+)
                (interpretArithmeticOperation op1)
                (interpretArithmeticOperation op2)

        Negate op ->
            Result.map negate
                (interpretArithmeticOperation op)

        Mul op1 op2 ->
            Result.map2
                (*)
                (interpretArithmeticOperation op1)
                (interpretArithmeticOperation op2)

        Div op1 op2 ->
            Result.map2
                (/)
                (interpretArithmeticOperation op1)
                (interpretArithmeticOperation op2)

        Max op1 op2 ->
            Result.map2
                max
                (interpretArithmeticOperation op1)
                (interpretArithmeticOperation op2)

        Min op1 op2 ->
            Result.map2
                min
                (interpretArithmeticOperation op1)
                (interpretArithmeticOperation op2)

        Condition boolOp op1 op2 ->
            -- Do not use map2 to defer evaluation in "then" or "else" branch.
            if interpretBooleanOperation boolOp then
                interpretArithmeticOperation op1
            else
                interpretArithmeticOperation op2

        ScaleEvaluation scale op ->
            Result.map (\v -> interpretScale v scale)
                (interpretArithmeticOperation op)

        ArithmeticError str op ->
            Err (str ++ ": " ++ (toString (interpretArithmeticOperation op)))


interpretBooleanOperation : BooleanOperation -> Bool
interpretBooleanOperation op =
    case op of
        Boolean bool ->
            bool

        And op1 op2 ->
            interpretBooleanOperation op1 && interpretBooleanOperation op2

        Or op1 op2 ->
            interpretBooleanOperation op1 || interpretBooleanOperation op2

        Equals op1 op2 ->
            interpretArithmeticOperation op1 == interpretArithmeticOperation op2


interpretScale : Float -> Scale -> Float
interpretScale value scale =
    let
        matchingBrackets =
            scale
                |> List.reverse
                |> List.filter (\( threshold, _ ) -> value > (Types.value threshold))
    in
        List.map2
            (\( threshold, rate ) previousThreshold ->
                let
                    thresholdValue =
                        Types.value threshold
                in
                    case previousThreshold of
                        Nothing ->
                            (value - thresholdValue) * rate

                        Just previousThreshold ->
                            ((Types.value previousThreshold) - thresholdValue) * rate
            )
            matchingBrackets
            (Nothing
                :: (List.map (\( threshold, _ ) -> Just threshold)
                        (List.take ((List.length matchingBrackets) - 1) matchingBrackets)
                   )
            )
            |> List.sum
