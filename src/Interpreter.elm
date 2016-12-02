module Interpreter exposing (..)

import Operations exposing (..)
import Types exposing (..)


interpretArithmeticOperation : ArithmeticOperation -> Float
interpretArithmeticOperation op =
    case op of
        Number float ->
            float

        Add op1 op2 ->
            (interpretArithmeticOperation op1) + (interpretArithmeticOperation op2)

        Negate op ->
            -(interpretArithmeticOperation op)

        Mul op1 op2 ->
            (interpretArithmeticOperation op1) * (interpretArithmeticOperation op2)

        Div op1 op2 ->
            (interpretArithmeticOperation op1) / (interpretArithmeticOperation op2)

        Max op1 op2 ->
            let
                v1 =
                    interpretArithmeticOperation op1

                v2 =
                    interpretArithmeticOperation op2
            in
                if v1 < v2 then
                    v2
                else
                    v1

        Min op1 op2 ->
            let
                v1 =
                    interpretArithmeticOperation op1

                v2 =
                    interpretArithmeticOperation op2
            in
                if v1 < v2 then
                    v1
                else
                    v2

        Condition boolOp op1 op2 ->
            if interpretBooleanOperation boolOp then
                interpretArithmeticOperation op1
            else
                interpretArithmeticOperation op2

        ScaleEvaluation scale op ->
            interpretScale (interpretArithmeticOperation op) scale


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
