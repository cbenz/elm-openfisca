module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Numeric
import Operations exposing (..)
import Plot exposing (..)
import Scale
import Senegal
import String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { conjointADesRevenus : BooleanOperation
    , estMarie : BooleanOperation
    , nbEnfants : ArithmeticOperation
    , salaire : ArithmeticOperation
    }


initialModel : Model
initialModel =
    { conjointADesRevenus = Boolean False
    , estMarie = Boolean False
    , nbEnfants = Number 0
    , salaire = Number 630000
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
        bareme2013 =
            -- Senegal.baremeImpotProgressif2013
            Scale.atDate "2013-01-01" Senegal.baremeImpotProgressif

        impotRevenusOperation =
            Senegal.impotRevenus model.estMarie model.conjointADesRevenus model.nbEnfants model.salaire bareme2013

        impotProgressifOperation =
            ScaleEvaluation bareme2013 model.salaire

        nbPartsOperation =
            Senegal.nbParts model.estMarie model.conjointADesRevenus model.nbEnfants
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
                        [ Html.Attributes.min "0"
                        , onInput SetSalaire
                        , step "1000"
                        , type_ "number"
                        , Html.Attributes.value
                            (model.salaire
                                |> evalArithmeticOperation
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
                        [ checked (evalBooleanOperation model.estMarie)
                        , onCheck SetEstMarie
                        , type_ "checkbox"
                        ]
                        []
                    , text "est marié"
                    ]
                , br [] []
                , label []
                    [ input
                        [ checked (evalBooleanOperation model.conjointADesRevenus)
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
                        [ Html.Attributes.min "0"
                        , onInput SetNbEnfants
                        , type_ "number"
                        , Html.Attributes.value
                            (model.nbEnfants
                                |> evalArithmeticOperation
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
                                |> evalArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , p []
                [ text
                    ("impôt progressif = "
                        ++ (impotProgressifOperation
                                |> evalArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , p []
                [ text
                    ("réductions pour charge de famille = "
                        ++ (Senegal.reductionImpotsPourChargeFamille impotProgressifOperation nbPartsOperation
                                |> evalArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , p []
                [ text
                    ("impôt = "
                        ++ (impotRevenusOperation
                                |> evalArithmeticOperation
                                |> toString
                           )
                    )
                ]
            , hr [] []
            , div []
                [ h2 [] [ text "Variation of salary on X axis" ]
                , viewPlot
                    [ ( ScaleEvaluation bareme2013, "blue" )
                    , ( \salaire ->
                            Senegal.impotRevenus
                                model.estMarie
                                model.conjointADesRevenus
                                model.nbEnfants
                                salaire
                                bareme2013
                      , "red"
                      )
                    , ( (\salaire ->
                            Senegal.reductionImpotsPourChargeFamille
                                (ScaleEvaluation bareme2013 salaire)
                                nbPartsOperation
                        )
                      , "green"
                      )
                    ]
                ]
            , hr [] []
            , p
                []
                [ text "viewArithmeticOperation impotRevenusOperation"
                , viewArithmeticOperation [] impotRevenusOperation
                ]
              -- , hr [] []
              -- , ul
              --     []
              --     (List.map
              --         (\scale ->
              --             li []
              --                 [ Scale.view scale ]
              --         )
              --         model.scales
              --     )
              -- , ul []
              --     (List.map
              --         (\scaleWithDate ->
              --             ul []
              --                 (List.map
              --                     (\date ->
              --                         li []
              --                             (let
              --                                 scale =
              --                                     scaleWithDate |> Scale.atDate date
              --                              in
              --                                 [ Scale.view scale ]
              --                             )
              --                     )
              --                     [ "2014-01-01", "2015-01-01" ]
              --                 )
              --         )
              --         model.timeChangingScales
              --     )
            ]


viewPlot : List ( ArithmeticOperation -> ArithmeticOperation, String ) -> Html msg
viewPlot funcs =
    let
        salaires =
            Numeric.linspace 0 16000000 100

        points func =
            List.map
                (\salaire ->
                    ( salaire
                    , func (Number salaire)
                        |> evalArithmeticOperation
                        |> Result.withDefault 0
                    )
                )
                salaires
    in
        plot [ plotStyle [ ( "padding", "0 0 2em 5em" ) ] ]
            ([ xAxis []
             , yAxis []
             ]
                ++ (List.map
                        (\( func, color ) ->
                            line [ lineStyle [ ( "fill", "none" ), ( "stroke", color ) ] ]
                                (points func)
                        )
                        funcs
                   )
            )
