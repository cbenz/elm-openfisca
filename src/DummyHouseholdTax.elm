module DummyHouseholdTax exposing (..)

import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Html exposing (..)
import Numeric
import Plot exposing (..)
import Scale exposing (..)
import Types exposing (..)


-- FORMULAS


irppScale : ScaleWithDates EUR
irppScale =
    [ { thresholds =
            [ ( "2014-01-01", "2015-12-31", EUR 0 )
            ]
      , rates =
            [ ( "2014-01-01", "2015-12-31", Rate 0 )
            ]
      }
    , { thresholds =
            [ ( "2014-01-01", "2014-12-31", EUR 6011 )
            , ( "2015-01-01", "2015-12-31", EUR 9690 )
            ]
      , rates =
            [ ( "2014-01-01", "2014-12-31", Rate 0.055 )
            , ( "2015-01-01", "2015-12-31", Rate 0.14 )
            ]
      }
    , { thresholds =
            [ ( "2014-01-01", "2014-12-31", EUR 11991 )
            , ( "2015-01-01", "2015-12-31", EUR 26764 )
            ]
      , rates =
            [ ( "2014-01-01", "2014-12-31", Rate 0.14 )
            , ( "2015-01-01", "2015-12-31", Rate 0.3 )
            ]
      }
    , { thresholds =
            [ ( "2014-01-01", "2014-12-31", EUR 26631 )
            , ( "2015-01-01", "2015-12-31", EUR 71754 )
            ]
      , rates =
            [ ( "2014-01-01", "2014-12-31", Rate 0.3 )
            , ( "2015-01-01", "2015-12-31", Rate 0.41 )
            ]
      }
    , { thresholds =
            [ ( "2014-01-01", "2014-12-31", EUR 71397 )
            , ( "2015-01-01", "2015-12-31", EUR 151956 )
            ]
      , rates =
            [ ( "2014-01-01", "2014-12-31", Rate 0.41 )
            , ( "2015-01-01", "2015-12-31", Rate 0.45 )
            ]
      }
    , { thresholds =
            [ ( "2014-01-01", "2014-12-31", EUR 151200 )
            ]
      , rates =
            [ ( "2014-01-01", "2014-12-31", Rate 0.45 )
            ]
      }
    ]


allocationLogement : Year -> YearMultiSerie EUR -> EUR
allocationLogement year salairesMenage =
    salairesMenage year
        |> List.map (\(EUR float) -> float)
        |> List.sum
        |> (\salairesSum ->
                EUR
                    (if salairesSum < 30000 then
                        2000
                     else
                        0
                    )
           )


irpp : Year -> YearMultiSerie EUR -> EUR
irpp (Year year) salairesFoyerFiscal =
    salairesFoyerFiscal (Year (year - 1))
        |> List.map (\(EUR float) -> float)
        |> List.sum
        |> (\salairesSum ->
                Scale.atDate ((toString year) ++ "-01-01") irppScale
                    |> Scale.compute (EUR salairesSum) (\(EUR x) -> x) EUR
           )


revenuDisponible : Year -> YearMultiSerie EUR -> YearMultiSerie EUR -> EUR
revenuDisponible year salairesMenage salairesFoyerFiscal =
    (irpp year salairesFoyerFiscal) €+ (allocationLogement year salairesMenage)



-- MODEL


type EUR
    = EUR Float


(€+) : EUR -> EUR -> EUR
(€+) (EUR a) (EUR b) =
    EUR (a + b)


type alias Individual =
    { salaire : Dict Int EUR }


type IndividualsGroup
    = FoyerFiscal String
    | Menage String


type alias Relationships =
    EveryDict IndividualsGroup (List Individual)


type alias Model =
    { individuals : Dict String Individual
    , relationships : Relationships
    }


initialModel : Model
initialModel =
    let
        individualA : Individual
        individualA =
            { salaire =
                Dict.fromList
                    [ ( 2014, EUR 40000 )
                    , ( 2015, EUR 40000 )
                    ]
            }

        individualB : Individual
        individualB =
            { salaire =
                Dict.fromList
                    [ ( 2014, EUR 10000 )
                    , ( 2015, EUR 15000 )
                    ]
            }

        individualC : Individual
        individualC =
            { salaire =
                Dict.fromList
                    [ ( 2014, EUR 20000 )
                    , ( 2015, EUR 15000 )
                    ]
            }
    in
        { individuals =
            Dict.fromList
                [ ( "individualA", individualA )
                , ( "individualB", individualB )
                , ( "individualC", individualC )
                ]
        , relationships =
            EveryDict.fromList
                [ ( FoyerFiscal "1", [ individualA, individualB, individualC ] )
                , ( Menage "1", [ individualA, individualB ] )
                , ( Menage "2", [ individualC ] )
                ]
        }



-- VIEW


view : Model -> Html msg
view model =
    let
        year2015 =
            Year 2015

        scaleForYear =
            Scale.atYearStart year2015 irppScale

        salaires : IndividualsGroup -> YearMultiSerie EUR
        salaires individualsGroup (Year year) =
            case EveryDict.get individualsGroup model.relationships of
                Nothing ->
                    []

                Just individuals ->
                    List.filterMap (.salaire >> Dict.get year) individuals

        allocationLogement2015Menage1 : EUR
        allocationLogement2015Menage1 =
            allocationLogement year2015 (salaires (Menage "1"))

        allocationLogement2015Menage2 : EUR
        allocationLogement2015Menage2 =
            allocationLogement year2015 (salaires (Menage "2"))

        irpp2015 : EUR
        irpp2015 =
            irpp year2015 (salaires (FoyerFiscal "1"))

        revenuDisponible2015Menage1 : EUR
        revenuDisponible2015Menage1 =
            revenuDisponible year2015
                (salaires (Menage "1"))
                (salaires (FoyerFiscal "1"))

        revenuDisponible2015Menage2 : EUR
        revenuDisponible2015Menage2 =
            revenuDisponible year2015
                (salaires (Menage "2"))
                (salaires (FoyerFiscal "1"))
    in
        div []
            [ ul []
                (model.individuals
                    |> Dict.toList
                    |> List.map (\( name, salaires ) -> li [] [ text (name ++ ": " ++ (toString salaires)) ])
                )
            , p [] [ Scale.view (\(EUR x) -> x) scaleForYear ]
            , p [] [ text ("irpp (Year 2015) (FoyerFiscal \"1\") = " ++ (toString irpp2015)) ]
            , p []
                [ text
                    ("allocationLogement (Year 2015) (Menage \"1\") = "
                        ++ (toString allocationLogement2015Menage1)
                    )
                ]
            , p []
                [ text
                    ("allocationLogement (Year 2015) (Menage \"2\") = "
                        ++ (toString allocationLogement2015Menage2)
                    )
                ]
            , p []
                [ text
                    ("revenuDisponible (Year 2015) (Menage \"1\") (FoyerFiscal \"1\") = "
                        ++ (toString revenuDisponible2015Menage1)
                    )
                ]
            , p []
                [ text
                    ("revenuDisponible (Year 2015) (Menage \"2\") (FoyerFiscal \"1\") = "
                        ++ (toString revenuDisponible2015Menage2)
                    )
                ]
            , viewPlot
                [ ( \salaire ->
                        let
                            (EUR irppFloat) =
                                irpp year2015 (constantMultiSerie (EUR salaire))
                        in
                            irppFloat
                  , "blue"
                  )
                , ( \salaire ->
                        let
                            (EUR allocationLogementFloat) =
                                allocationLogement year2015 (constantMultiSerie (EUR salaire))
                        in
                            allocationLogementFloat
                  , "green"
                  )
                , ( \salaire ->
                        let
                            (EUR revenuDisponibleFloat) =
                                revenuDisponible year2015
                                    (constantMultiSerie (EUR salaire))
                                    (constantMultiSerie (EUR salaire))
                        in
                            revenuDisponibleFloat
                  , "red"
                  )
                ]
            ]


viewPlot : List ( Float -> Float, String ) -> Html msg
viewPlot funcs =
    let
        salaires =
            Numeric.linspace 0 200000 1000

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
            ([ xAxis [], yAxis [] ]
                ++ (List.map
                        (\( func, color ) ->
                            line [ lineStyle [ ( "fill", "none" ), ( "stroke", color ) ] ]
                                (points func)
                        )
                        funcs
                   )
            )
