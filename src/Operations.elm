module Operations exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
                Basics.max
                (evalArithmeticOperation op1)
                (evalArithmeticOperation op2)

        Min op1 op2 ->
            Result.map2
                Basics.min
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



-- VIEW


viewArithmeticOperation : List (Html.Attribute msg) -> ArithmeticOperation -> Html msg
viewArithmeticOperation attrs op =
    let
        viewChildren str ops =
            text str :: List.map (viewArithmeticOperation []) ops
    in
        ul attrs
            [ li []
                (case op of
                    Number n ->
                        [ text ("Number " ++ (toString n)) ]

                    Add op1 op2 ->
                        viewChildren "Add" [ op1, op2 ]

                    Negate op ->
                        viewChildren "Negate" [ op ]

                    Mul op1 op2 ->
                        viewChildren "Mul" [ op1, op2 ]

                    Div op1 op2 ->
                        viewChildren "Div" [ op1, op2 ]

                    Max op1 op2 ->
                        viewChildren "Max" [ op1, op2 ]

                    Min op1 op2 ->
                        viewChildren "Min" [ op1, op2 ]

                    Condition boolOp op1 op2 ->
                        let
                            condition =
                                evalBooleanOperation boolOp
                        in
                            [ text "Condition"
                            , br [] []
                            , text "if"
                            , viewBooleanOperation [] boolOp
                            , text "then"
                            , viewArithmeticOperation
                                [ style
                                    (if condition then
                                        [ ( "color", "lightgray" ) ]
                                     else
                                        []
                                    )
                                ]
                                op1
                            , text "else"
                            , viewArithmeticOperation
                                [ style
                                    (if condition then
                                        []
                                     else
                                        [ ( "color", "lightgray" ) ]
                                    )
                                ]
                                op2
                            ]

                    ScaleEvaluation scale op ->
                        [ text "ScaleEvaluation"
                        , br [] []
                        , text
                            (evalArithmeticOperation op
                                |> Result.map (interpretScale scale)
                                |> toString
                            )
                        ]

                    ArithmeticError str op ->
                        [ div [ style [ ( "color", "red" ) ] ]
                            [ text (str ++ ": " ++ (toString (evalArithmeticOperation op))) ]
                        ]
                )
            ]


viewBooleanOperation : List (Html.Attribute msg) -> BooleanOperation -> Html msg
viewBooleanOperation attrs op =
    let
        viewChildren str ops =
            text str :: List.map (viewBooleanOperation []) ops
    in
        ul attrs
            [ li []
                (case op of
                    Boolean value ->
                        [ text ("Boolean " ++ (toString value)) ]

                    And op1 op2 ->
                        viewChildren "And" [ op1, op2 ]

                    Or op1 op2 ->
                        viewChildren "Or" [ op1, op2 ]

                    Equals op1 op2 ->
                        text "Equals" :: List.map (viewArithmeticOperation []) [ op1, op2 ]
                )
            ]
