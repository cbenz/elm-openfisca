module DummyHouseholdTax exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Scale exposing (..)
import Types exposing (..)


type EUR
    = EUR Float


(€+) : EUR -> EUR -> EUR
(€+) (EUR a) (EUR b) =
    EUR (a + b)


type Entity
    = FoyerFiscal String
    | Menage String



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


type alias Model =
    { salairesEnfant1 : Dict Int EUR
    , salairesParent1 : Dict Int EUR
    , salairesParent2 : Dict Int EUR
    }


initialModel : Model
initialModel =
    { salairesParent1 =
        Dict.fromList
            [ ( 2014, EUR 40000 )
            , ( 2015, EUR 40000 )
            ]
    , salairesParent2 =
        Dict.fromList
            [ ( 2014, EUR 10000 )
            , ( 2015, EUR 15000 )
            ]
    , salairesEnfant1 =
        Dict.fromList
            [ ( 2014, EUR 20000 )
            , ( 2015, EUR 15000 )
            ]
    }


salaireFromData : Dict Int EUR -> Year -> Maybe EUR
salaireFromData salaires (Year year) =
    Dict.get year salaires



-- VIEW


view : Model -> Html msg
view model =
    let
        salaireParent1 =
            salaireFromData model.salairesParent1

        salaireParent2 =
            salaireFromData model.salairesParent2

        salaireEnfant1 =
            salaireFromData model.salairesEnfant1

        salaires : Entity -> Year -> List EUR
        salaires entity year =
            -- TODO Make this function declarative by storing individuals relationships into the model as data
            (case entity of
                FoyerFiscal "1" ->
                    [ salaireParent1 year
                    , salaireParent2 year
                    , salaireEnfant1 year
                    ]

                Menage "1" ->
                    [ salaireParent1 year
                    , salaireParent2 year
                    ]

                Menage "2" ->
                    [ salaireEnfant1 year ]

                _ ->
                    -- TODO Better handle absence of value (return Maybe?)
                    []
            )
                |> List.filterMap identity

        year2015 =
            Year 2015

        scaleForYear =
            Scale.atYearStart year2015 irppScale

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
            [ p [] [ text ("Salaires annuels parent_1: " ++ (toString model.salairesParent1)) ]
            , p [] [ text ("Salaires annuels parent_2 : " ++ (toString model.salairesParent2)) ]
            , p [] [ text ("Salaires annuels enfant_1 : " ++ (toString model.salairesEnfant1)) ]
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
            ]
