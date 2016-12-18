module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Numeric
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
    { conjointADesRevenus : Bool
    , estMarie : Bool
    , nbEnfants : Int
    , salaire : Float
    }


initialModel : Model
initialModel =
    { conjointADesRevenus = False
    , estMarie = False
    , nbEnfants = 0
    , salaire = 630000
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
            ( { model | conjointADesRevenus = bool }
            , Cmd.none
            )

        SetEstMarie bool ->
            ( { model | estMarie = bool }
            , Cmd.none
            )

        SetNbEnfants str ->
            let
                newModel =
                    if String.isEmpty str then
                        { model | nbEnfants = 0 }
                    else
                        case String.toInt str of
                            Ok int ->
                                { model | nbEnfants = int }

                            Err _ ->
                                model
            in
                ( newModel, Cmd.none )

        SetSalaire str ->
            let
                newModel =
                    if String.isEmpty str then
                        { model | salaire = 0 }
                    else
                        case String.toFloat str of
                            Ok float ->
                                { model | salaire = float }

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

        impotRevenus =
            Senegal.impotRevenus model.estMarie model.conjointADesRevenus model.nbEnfants model.salaire bareme2013

        impotProgressif =
            Scale.compute model.salaire bareme2013

        nbParts =
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
                        , Html.Attributes.value (toString model.salaire)
                        ]
                        []
                    , text " francs CFA"
                    ]
                , br [] []
                , label []
                    [ input
                        [ checked model.estMarie
                        , onCheck SetEstMarie
                        , type_ "checkbox"
                        ]
                        []
                    , text "est marié"
                    ]
                , br [] []
                , label []
                    [ input
                        [ checked model.conjointADesRevenus
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
                        , Html.Attributes.value (toString model.nbEnfants)
                        ]
                        []
                    ]
                ]
            , p []
                [ text ("nb parts = " ++ (toString nbParts))
                ]
            , p []
                [ text
                    ("impôt progressif = " ++ (toString impotProgressif))
                ]
            , p []
                [ text
                    ("réductions pour charge de famille = "
                        ++ (Senegal.reductionImpotsPourChargeFamille impotProgressif nbParts
                                |> toString
                           )
                    )
                ]
            , p []
                [ text ("impôt = " ++ (toString impotRevenus))
                ]
            , hr [] []
            , div []
                [ h2 [] [ text "Variation of salary on X axis" ]
                , viewPlot
                    model.salaire
                    [ ( \salaire -> Scale.compute salaire bareme2013, "blue" )
                    , ( \salaire ->
                            Senegal.impotRevenus
                                model.estMarie
                                model.conjointADesRevenus
                                model.nbEnfants
                                salaire
                                bareme2013
                                |> Result.withDefault -1
                      , "red"
                      )
                    , ( (\salaire ->
                            Senegal.reductionImpotsPourChargeFamille
                                (Scale.compute salaire bareme2013)
                                nbParts
                                |> Result.withDefault -1
                        )
                      , "green"
                      )
                    ]
                ]
            ]


viewPlot : Float -> List ( Float -> Float, String ) -> Html msg
viewPlot x funcs =
    let
        salaires =
            Numeric.linspace 0 16000000 1000

        points func =
            List.map
                (\salaire ->
                    ( salaire
                    , func salaire
                    )
                )
                salaires
    in
        plot [ plotStyle [ ( "padding", "0 0 2em 5em" ) ] ]
            ([ xAxis []
             , yAxis []
             , line [ lineStyle [ ( "fill", "none" ), ( "stroke", "gray" ) ] ]
                [ ( x, 0 ), ( x, 5000000 ) ]
             ]
                ++ (List.map
                        (\( func, color ) ->
                            line [ lineStyle [ ( "fill", "none" ), ( "stroke", color ) ] ]
                                (points func)
                        )
                        funcs
                   )
            )
