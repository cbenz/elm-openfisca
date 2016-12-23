module Senegal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Numeric
import Plot exposing (..)
import Scale exposing (..)


type CFA
    = CFA Float



-- FORMULAS


nbParts : Bool -> Bool -> Int -> Float
nbParts estMarie conjointADesRevenus nbEnfants =
    let
        nbPartsIndividu =
            1

        nbPartsConjoint =
            (if estMarie then
                0.5
             else
                0
            )
                + (if conjointADesRevenus then
                    0
                   else
                    0.5
                  )

        nbPartsEnfants =
            (toFloat nbEnfants) * 0.5
    in
        List.sum [ nbPartsIndividu, nbPartsConjoint, nbPartsEnfants ]
            |> Basics.min 5


reductionImpotsPourChargeFamille : CFA -> Float -> Result String CFA
reductionImpotsPourChargeFamille (CFA impotProgressif) nbParts =
    let
        data =
            [ ( 1, { taux = 0, minimum = 0, maximum = 0 } )
            , ( 1.5, { taux = 0.1, minimum = 100000, maximum = 300000 } )
            , ( 2, { taux = 0.15, minimum = 200000, maximum = 650000 } )
            , ( 2.5, { taux = 0.2, minimum = 300000, maximum = 1100000 } )
            , ( 3, { taux = 0.25, minimum = 400000, maximum = 1650000 } )
            , ( 3.5, { taux = 0.3, minimum = 500000, maximum = 2030000 } )
            , ( 4, { taux = 0.35, minimum = 600000, maximum = 2490000 } )
            , ( 4.5, { taux = 0.4, minimum = 700000, maximum = 2755000 } )
            , ( 5, { taux = 0.45, minimum = 800000, maximum = 3180000 } )
            ]

        findValue xs getter =
            case xs of
                [] ->
                    Err ("nbParts = " ++ (toString nbParts) ++ " not found")

                ( nbPartsOfItem, values ) :: tail ->
                    if nbParts == nbPartsOfItem then
                        Ok (getter values)
                    else
                        findValue tail getter
    in
        Result.map3
            (\taux minimum maximum ->
                clamp minimum maximum (impotProgressif * taux)
            )
            (findValue data .taux)
            (findValue data .minimum)
            (findValue data .maximum)
            |> Result.map CFA


baremeImpotProgressif : ScaleWithDates CFA
baremeImpotProgressif =
    let
        start =
            "2013-01-01"

        stop =
            "2013-12-31"
    in
        scaleWithDates
            CFA
            [ { thresholds =
                    [ ( start, stop, 0 ) ]
              , rates =
                    [ ( start, stop, 0 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 630000 ) ]
              , rates =
                    [ ( start, stop, 0.2 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 1500000 ) ]
              , rates =
                    [ ( start, stop, 0.3 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 4000000 ) ]
              , rates =
                    [ ( start, stop, 0.35 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 8000000 ) ]
              , rates =
                    [ ( start, stop, 0.37 ) ]
              }
            , { thresholds =
                    [ ( start, stop, 13500000 ) ]
              , rates =
                    [ ( start, stop, 0.4 ) ]
              }
            ]


impotRevenus : Bool -> Bool -> Int -> CFA -> Scale CFA -> Result String CFA
impotRevenus estMarie conjointADesRevenus nbEnfants salaire bareme =
    let
        impotProgressif =
            Scale.compute
                salaire
                (\(CFA float) -> float)
                CFA
                bareme

        nbPartsOperation =
            nbParts estMarie conjointADesRevenus nbEnfants
    in
        reductionImpotsPourChargeFamille impotProgressif nbPartsOperation
            |> Result.map
                (\(CFA reductionImpotsPourChargeFamille) ->
                    let
                        (CFA impotProgressifFloat) =
                            impotProgressif
                    in
                        Basics.max 0 (impotProgressifFloat - reductionImpotsPourChargeFamille)
                )
            |> Result.map CFA



-- MODEL


type alias Model =
    { conjointADesRevenus : Bool
    , estMarie : Bool
    , nbEnfants : Int
    , salaire : CFA
    }


initialModel : Model
initialModel =
    { conjointADesRevenus = False
    , estMarie = False
    , nbEnfants = 0
    , salaire = CFA 630000
    }



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
                        { model | salaire = CFA 0 }
                    else
                        case String.toFloat str of
                            Ok float ->
                                { model | salaire = CFA float }

                            Err _ ->
                                model
            in
                ( newModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        baremeImpotProgressif2013 =
            Scale.atDate "2013-01-01" baremeImpotProgressif

        impotRevenusResult =
            impotRevenus
                model.estMarie
                model.conjointADesRevenus
                model.nbEnfants
                model.salaire
                baremeImpotProgressif2013

        toFloat (CFA f) =
            f

        impotProgressif =
            Scale.compute
                model.salaire
                toFloat
                CFA
                baremeImpotProgressif2013

        nbPartsFloat =
            nbParts model.estMarie model.conjointADesRevenus model.nbEnfants
    in
        div []
            [ Html.form []
                [ label []
                    [ text "Salaire annuel "
                    , input
                        [ Html.Attributes.min "0"
                        , onInput SetSalaire
                        , step "1000"
                        , type_ "number"
                        , Html.Attributes.value (model.salaire |> toFloat |> toString)
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
                [ text ("nb parts = " ++ (toString nbPartsFloat))
                ]
            , Scale.view baremeImpotProgressif2013 toFloat
            , p []
                [ text
                    ("impôt progressif = " ++ (toString impotProgressif))
                ]
            , p []
                [ text
                    ("réductions pour charge de famille = "
                        ++ (reductionImpotsPourChargeFamille impotProgressif nbPartsFloat
                                |> toString
                           )
                    )
                ]
            , p []
                [ text ("impôt = " ++ (toString impotRevenusResult))
                ]
            , viewPlot
                (toFloat model.salaire)
                [ ( \salaire ->
                        Scale.compute
                            (CFA salaire)
                            toFloat
                            CFA
                            baremeImpotProgressif2013
                            |> toFloat
                  , "blue"
                  )
                , ( \salaire ->
                        impotRevenus
                            model.estMarie
                            model.conjointADesRevenus
                            model.nbEnfants
                            (CFA salaire)
                            baremeImpotProgressif2013
                            |> Result.withDefault (CFA -1)
                            |> toFloat
                  , "red"
                  )
                , ( (\salaire ->
                        reductionImpotsPourChargeFamille
                            (Scale.compute
                                (CFA salaire)
                                toFloat
                                CFA
                                baremeImpotProgressif2013
                            )
                            nbPartsFloat
                            |> Result.withDefault (CFA -1)
                            |> toFloat
                    )
                  , "green"
                  )
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
