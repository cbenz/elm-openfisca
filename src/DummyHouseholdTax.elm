module DummyHouseholdTax exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Scale exposing (..)
import Types exposing (..)


type EUR
    = EUR Float



-- FORMULAS


irppScale : ScaleWithDates EUR
irppScale =
    scaleWithDates
        EUR
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


irpp : Year -> YearSerie EUR -> YearSerie EUR -> Maybe EUR
irpp (Year year) salaireIndividu salaireConjoint =
    let
        lastYear =
            Year (year - 1)

        scaleForYear =
            Scale.atDate ((toString year) ++ "-01-01") irppScale
    in
        Maybe.map2
            (\(EUR salaireIndividu) (EUR salaireConjoint) ->
                let
                    salaires =
                        EUR (salaireIndividu + salaireConjoint)
                in
                    Scale.compute salaires (\(EUR x) -> x) EUR scaleForYear
            )
            (salaireIndividu lastYear)
            (salaireConjoint lastYear)



-- MODEL


type alias Model =
    { salairesIndividu : Dict Int EUR
    , salairesConjoint : Dict Int EUR
    }


initialModel : Model
initialModel =
    { salairesIndividu =
        Dict.fromList
            [ ( 2014, EUR 40000 )
            ]
    , salairesConjoint =
        Dict.fromList
            [ ( 2014, EUR 10000 )
            ]
    }


salaireFromData : Dict Int EUR -> Year -> Maybe EUR
salaireFromData salaires =
    (\(Year year) -> Dict.get year salaires)



-- VIEW


view : Model -> Html msg
view model =
    let
        salaireIndividu =
            salaireFromData model.salairesIndividu

        salaireConjoint =
            salaireFromData model.salairesConjoint

        scaleForYear =
            Scale.atDate "2015-01-01" irppScale
    in
        div []
            [ p [] [ text ("Salaire par an individu : " ++ (toString model.salairesIndividu)) ]
            , p [] [ text ("Salaire par an conjoint: " ++ (toString model.salairesConjoint)) ]
            , p [] [ Scale.view (\(EUR x) -> x) scaleForYear ]
            , p [] [ text ("irpp 2015 = " ++ (toString (irpp (Year 2015) salaireIndividu salaireConjoint))) ]
            ]
