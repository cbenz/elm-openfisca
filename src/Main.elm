module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Languages.French
import Numeral


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type alias Currency =
    String


type ValueWithUnit
    = MonetaryAmount Currency Float
    | Rate Float
    | Amount Float


type Scale
    = Scale (List ( ValueWithUnit, ValueWithUnit ))


scale : (number -> ValueWithUnit) -> (number -> ValueWithUnit) -> List ( number, number ) -> Scale
scale thresholdTagger valueTagger brackets =
    Scale
        (List.map
            (\( threshold, value ) ->
                ( thresholdTagger threshold, valueTagger value )
            )
            brackets
        )


brackets : Scale -> List ( ValueWithUnit, ValueWithUnit )
brackets (Scale brackets) =
    brackets


formatUnit : ValueWithUnit -> String
formatUnit unit =
    case unit of
        Rate float ->
            Numeral.formatWithLanguage Languages.French.lang "0.[00] %" float

        MonetaryAmount _ float ->
            Numeral.formatWithLanguage Languages.French.lang "0.[00] a$" float

        Amount int ->
            toString int



-- MODEL


type alias Model =
    { scales : List Scale
    }


initialModel : Model
initialModel =
    let
        bareme_impot_senegal =
            scale
                (MonetaryAmount "CFA")
                Rate
                [ ( 0, 0 )
                , ( 630000, 0.2 )
                , ( 1500000, 0.3 )
                , ( 4000000, 0.35 )
                , ( 8000000, 0.37 )
                , ( 13500000, 0.4 )
                ]

        bareme_impot_france_2015 =
            scale
                (MonetaryAmount "€")
                Rate
                [ ( 0, 0 )
                , ( 9690, 0.14 )
                , ( 26764, 0.3 )
                , ( 71754, 0.41 )
                , ( 151956, 0.45 )
                ]

        bareme_reductions_pour_charge_de_famille =
            scale
                Amount
                Rate
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
        { scales =
            [ bareme_impot_senegal
            , bareme_impot_france_2015
            , bareme_reductions_pour_charge_de_famille
            ]
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html msg
view model =
    div []
        (List.map viewScale model.scales)


viewScale : Scale -> Html msg
viewScale scale =
    let
        scaleBrackets =
            brackets scale
    in
        tableWithBorders
            (List.map2
                (\( threshold, value ) nextThreshold ->
                    [ let
                        toThreshold threshold =
                            "jusqu'à " ++ (formatUnit threshold)
                      in
                        case ( threshold, nextThreshold ) of
                            ( MonetaryAmount _ 0, Just nextThreshold ) ->
                                text (toThreshold nextThreshold)

                            ( Amount 0, Just nextThreshold ) ->
                                text (toThreshold nextThreshold)

                            ( threshold, Just nextThreshold ) ->
                                text ("de " ++ (formatUnit threshold) ++ " à " ++ (formatUnit nextThreshold))

                            ( threshold, Nothing ) ->
                                text ("supérieur à " ++ (formatUnit threshold))
                    , text (formatUnit value)
                    ]
                )
                scaleBrackets
                ((List.drop 1 scaleBrackets
                    |> List.map (\( threshold, _ ) -> Just threshold)
                 )
                    ++ [ Nothing ]
                )
            )



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
