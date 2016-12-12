module Main exposing (..)

import Senegal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Interpreter
import Languages.French
import Numeral
import Operations exposing (..)
import String
import Types exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


timeChangingScale :
    (number -> ValueWithUnit)
    -> List
        { thresholds : List ( Date, Date, number )
        , rates : List ( Date, Date, Rate )
        }
    -> TimeChangingScale
timeChangingScale thresholdTagger brackets =
    List.map
        (\{ thresholds, rates } ->
            { thresholds =
                List.map
                    (\( start, stop, threshold ) -> ( start, stop, thresholdTagger threshold ))
                    thresholds
            , rates = rates
            }
        )
        brackets


atDate : Date -> TimeChangingScale -> Scale
atDate date scale =
    let
        findForDate xs =
            List.filterMap
                (\( start, stop, x ) ->
                    if start <= date && date <= stop then
                        Just x
                    else
                        Nothing
                )
                xs
                |> List.head
    in
        List.filterMap
            (\{ thresholds, rates } ->
                Maybe.map2 (,) (findForDate thresholds) (findForDate rates)
            )
            scale


formatValueWithUnit : ValueWithUnit -> String
formatValueWithUnit unit =
    case unit of
        MonetaryAmount _ float ->
            Numeral.formatWithLanguage Languages.French.lang "0.[00] a$" float

        Amount int ->
            toString int



-- MODEL


type alias Model =
    { conjointADesRevenus : BooleanOperation
    , estMarie : BooleanOperation
    , nbEnfants : ArithmeticOperation
    , salaire : ArithmeticOperation
    , scales : List Scale
    , timeChangingScales : List TimeChangingScale
    }


initialModel : Model
initialModel =
    let
        baremeImpotFrance2014 =
            scale
                (MonetaryAmount "€")
                [ ( 0, 0 )
                , ( 6011, 0.055 )
                , ( 11991, 0.14 )
                , ( 26631, 0.3 )
                , ( 71397, 0.41 )
                , ( 151200, 0.45 )
                ]

        baremeImpotFrance2015 =
            scale
                (MonetaryAmount "€")
                [ ( 0, 0 )
                , ( 9690, 0.14 )
                , ( 26764, 0.3 )
                , ( 71754, 0.41 )
                , ( 151956, 0.45 )
                ]

        baremeImpotFrance =
            timeChangingScale
                (MonetaryAmount "€")
                [ { thresholds =
                        [ ( "2014-01-01", "2015-12-31", 0 )
                        ]
                  , rates =
                        [ ( "2014-01-01", "2015-12-31", 0 )
                        ]
                  }
                , { thresholds =
                        [ ( "2014-01-01", "2014-12-31", 6011 )
                        , ( "2015-01-01", "2015-12-31", 9690 )
                        ]
                  , rates =
                        [ ( "2014-01-01", "2014-12-31", 0.055 )
                        , ( "2015-01-01", "2015-12-31", 0.14 )
                        ]
                  }
                , { thresholds =
                        [ ( "2014-01-01", "2014-12-31", 11991 )
                        , ( "2015-01-01", "2015-12-31", 26764 )
                        ]
                  , rates =
                        [ ( "2014-01-01", "2014-12-31", 0.14 )
                        , ( "2015-01-01", "2015-12-31", 0.3 )
                        ]
                  }
                , { thresholds =
                        [ ( "2014-01-01", "2014-12-31", 26631 )
                        , ( "2015-01-01", "2015-12-31", 71754 )
                        ]
                  , rates =
                        [ ( "2014-01-01", "2014-12-31", 0.3 )
                        , ( "2015-01-01", "2015-12-31", 0.41 )
                        ]
                  }
                , { thresholds =
                        [ ( "2014-01-01", "2014-12-31", 71397 )
                        , ( "2015-01-01", "2015-12-31", 151956 )
                        ]
                  , rates =
                        [ ( "2014-01-01", "2014-12-31", 0.41 )
                        , ( "2015-01-01", "2015-12-31", 0.45 )
                        ]
                  }
                , { thresholds =
                        [ ( "2014-01-01", "2014-12-31", 151200 )
                        ]
                  , rates =
                        [ ( "2014-01-01", "2014-12-31", 0.45 )
                        ]
                  }
                ]

        baremeReductionsPourChargeDeFamille =
            scale
                Amount
                [ ( 1, 0 )
                , ( 1.5, 0.1 )
                , ( 2, 0.15 )
                , ( 2.5, 0.2 )
                , ( 3, 0.25 )
                , ( 3.5, 0.3 )
                , ( 4, 0.35 )
                , ( 4.5, 0.4 )
                , ( 5, 0.45 )
                ]
    in
        { conjointADesRevenus = Boolean False
        , estMarie = Boolean False
        , nbEnfants = Number 0
        , salaire = Number 630000
        , scales =
            [ baremeImpotFrance2014
            , baremeImpotFrance2015
            , Senegal.baremeImpotProgressif2013
            , baremeReductionsPourChargeDeFamille
            ]
        , timeChangingScales =
            [ baremeImpotFrance
            ]
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = SetConjointADesRevenus Bool
    | SetEstMarie Bool
    | SetNbEnfants String
    | SetSalaire String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetConjointADesRevenus bool ->
            ( { model | conjointADesRevenus = Boolean bool }
            , Cmd.none
            )

        SetEstMarie bool ->
            ( { model | estMarie = Boolean bool }
            , Cmd.none
            )

        SetNbEnfants str ->
            let
                newModel =
                    if String.isEmpty str then
                        { model | nbEnfants = Number 0 }
                    else
                        case String.toInt str of
                            Ok int ->
                                { model | nbEnfants = Number (toFloat int) }

                            Err _ ->
                                model
            in
                ( newModel, Cmd.none )

        SetSalaire str ->
            let
                newModel =
                    if String.isEmpty str then
                        { model | salaire = Number 0 }
                    else
                        case String.toFloat str of
                            Ok float ->
                                { model | salaire = Number float }

                            Err _ ->
                                model
            in
                ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        impotRevenusOperation =
            impotRevenus model.salaire model.estMarie model.conjointADesRevenus model.nbEnfants

        impotProgressifOperation =
            ScaleEvaluation baremeImpotProgressif2013 model.salaire

        nbPartsOperation =
            nbParts model.estMarie model.conjointADesRevenus model.nbEnfants
    in
        div [ style [ ( "margin", "1em" ) ] ]
            [ h1 [] [ text "elm-openfisca playground" ]
            , p []
                [ text "Source code:"
                , let
                    url =
                        "https://github.com/cbenz/elm-openfisca"
                  in
                    a [ href url ] [ text url ]
                ]
            , hr [] []
            , Html.form []
                [ label []
                    [ text "Salaire annuel "
                    , input
                        [ onInput SetSalaire
                        , step "1000"
                        , type_ "number"
                        , Html.Attributes.value
                            (model.salaire
                                |> Interpreter.interpretArithmeticOperation
                                |> Result.withDefault 0
                                |> toString
                            )
                        ]
                        []
                    , text " francs CFA"
                    ]
                , br [] []
                , label []
                    [ input
                        [ checked (Interpreter.interpretBooleanOperation model.estMarie)
                        , onCheck SetEstMarie
                        , type_ "checkbox"
                        ]
                        []
                    , text "est marié"
                    ]
                , br [] []
                , label []
                    [ input
                        [ checked (Interpreter.interpretBooleanOperation model.conjointADesRevenus)
                        , onCheck SetConjointADesRevenus
                        , type_ "checkbox"
                        ]
                        []
                    , text "conjoint a des revenus"
                    ]
                , br [] []
                , label []
                    [ text "Nb enfants "
                    , input
                        [ onInput SetNbEnfants
                        , type_ "number"
                        , Html.Attributes.value
                            (model.nbEnfants
                                |> Interpreter.interpretArithmeticOperation
                                |> Result.withDefault 0
                                |> toString
                            )
                        ]
                        []
                    ]
                ]
            , p []
                [ text
                    ("nb parts = "
                        ++ (nbPartsOperation
                                |> Interpreter.interpretArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , p []
                [ text
                    ("impôt progressif = "
                        ++ (impotProgressifOperation
                                |> Interpreter.interpretArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , p []
                [ text
                    ("réductions pour charge de famille = "
                        ++ (reductionImpotsPourChargeFamille impotProgressifOperation nbPartsOperation
                                |> Interpreter.interpretArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , p []
                [ text
                    ("impôt = "
                        ++ (impotRevenusOperation
                                |> Interpreter.interpretArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , hr [] []
            , p
                []
                [ text "viewArithmeticOperation impotRevenusOperation"
                , viewArithmeticOperation impotRevenusOperation
                ]
            , ul
                []
                (List.map
                    (\scale ->
                        li []
                            [ viewScale scale ]
                    )
                    model.scales
                )
            , ul []
                (List.map
                    (\timeChangingScale ->
                        ul []
                            (List.map
                                (\date ->
                                    li []
                                        (let
                                            scale =
                                                timeChangingScale |> atDate date
                                         in
                                            [ viewScale scale ]
                                         -- ++ (List.map
                                         --         (\value ->
                                         --             p []
                                         --                 [ text
                                         --                     ("For "
                                         --                         ++ (formatValueWithUnit value)
                                         --                         ++ ": "
                                         --                         ++ (formatValueWithUnit (calculate value scale))
                                         --                     )
                                         --                 ]
                                         --         )
                                         --         ([ 1000, 15000, 50000 ] |> List.map (MonetaryAmount "€"))
                                         --    )
                                        )
                                )
                                [ "2014-01-01", "2015-01-01" ]
                            )
                    )
                    model.timeChangingScales
                )
            ]


viewArithmeticOperation : ArithmeticOperation -> Html msg
viewArithmeticOperation op =
    let
        viewChildren s op1 op2 =
            div []
                [ text s
                , ul []
                    [ li [] [ viewArithmeticOperation op1 ]
                    , li [] [ viewArithmeticOperation op2 ]
                    ]
                ]
    in
        case op of
            Number n ->
                text (toString n)

            Add op1 op2 ->
                viewChildren "Add" op1 op2

            Negate op ->
                div []
                    [ text "Negate"
                    , ul [] [ li [] [ viewArithmeticOperation op ] ]
                    ]

            Mul op1 op2 ->
                viewChildren "Mul" op1 op2

            Div op1 op2 ->
                viewChildren "Div" op1 op2

            Max op1 op2 ->
                viewChildren "Max" op1 op2

            Min op1 op2 ->
                viewChildren "Min" op1 op2

            Condition boolOp op1 op2 ->
                div []
                    [ text "Condition"
                    , ul []
                        [ li [] [ text "(if) ", viewBooleanOperation boolOp ]
                        , li [] [ text "(then) ", viewArithmeticOperation op1 ]
                        , li [] [ text "(else) ", viewArithmeticOperation op2 ]
                        ]
                    ]

            ScaleEvaluation scale op ->
                div []
                    [ text "ScaleEvaluation"
                    , ul []
                        [ viewScale scale
                        , viewArithmeticOperation op
                        ]
                    ]

            ArithmeticError str op ->
                div [ style [ ( "color", "red" ) ] ]
                    [ text (str ++ ": " ++ (toString (Interpreter.interpretArithmeticOperation op))) ]


viewBooleanOperation : BooleanOperation -> Html msg
viewBooleanOperation op =
    case op of
        Boolean value ->
            text (toString value)

        And op1 op2 ->
            div []
                [ text "And"
                , ul []
                    [ li [] [ viewBooleanOperation op1 ]
                    , li [] [ viewBooleanOperation op2 ]
                    ]
                ]

        Or op1 op2 ->
            div []
                [ text "Or"
                , ul []
                    [ li [] [ viewBooleanOperation op1 ]
                    , li [] [ viewBooleanOperation op2 ]
                    ]
                ]

        Equals op1 op2 ->
            div []
                [ text "Equals"
                , ul []
                    [ li [] [ viewArithmeticOperation op1 ]
                    , li [] [ viewArithmeticOperation op2 ]
                    ]
                ]


viewScale : Scale -> Html msg
viewScale scale =
    div []
        [ tableWithBorders
            (List.map2
                (\( threshold, rate ) nextThreshold ->
                    [ let
                        toThreshold threshold =
                            "jusqu'à " ++ (formatValueWithUnit threshold)
                      in
                        case ( threshold, nextThreshold ) of
                            ( MonetaryAmount _ 0, Just nextThreshold ) ->
                                text (toThreshold nextThreshold)

                            ( Amount 0, Just nextThreshold ) ->
                                text (toThreshold nextThreshold)

                            ( threshold, Just nextThreshold ) ->
                                text ("de " ++ (formatValueWithUnit threshold) ++ " à " ++ (formatValueWithUnit nextThreshold))

                            ( threshold, Nothing ) ->
                                text ("supérieur à " ++ (formatValueWithUnit threshold))
                    , text
                        (Numeral.formatWithLanguage
                            Languages.French.lang
                            "0.[00] %"
                            rate
                        )
                    ]
                )
                scale
                ((List.drop 1 scale
                    |> List.map (\( threshold, _ ) -> Just threshold)
                 )
                    ++ [ Nothing ]
                )
            )
        ]



-- HTML HELPERS


tableWithBorders : List (List (Html msg)) -> Html msg
tableWithBorders rows =
    table
        [ style
            [ ( "border", "1px solid" )
            , ( "border-collapse", "collapse" )
            ]
        ]
        [ tbody []
            (rows
                |> List.map
                    (\cells ->
                        tr []
                            (cells
                                |> List.map
                                    (\cell ->
                                        td
                                            [ style
                                                [ ( "border", "1px solid" )
                                                , ( "padding", "1em" )
                                                ]
                                            ]
                                            [ cell ]
                                    )
                            )
                    )
            )
        ]
