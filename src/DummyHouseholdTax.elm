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


allocationLogement : Year -> YearSerie EUR -> YearSerie EUR -> Maybe EUR
allocationLogement year salaireIndividu salaireConjoint =
    Maybe.map2
        (\(EUR salaireIndividuForYear) (EUR salaireConjointForYear) ->
            EUR
                (if salaireIndividuForYear + salaireConjointForYear < 30000 then
                    2000
                 else
                    0
                )
        )
        (salaireIndividu year)
        (salaireConjoint year)


irpp : Year -> YearSerie EUR -> YearSerie EUR -> YearSerie EUR -> Maybe EUR
irpp (Year year) salaireIndividu salaireConjoint salaireEnfant1 =
    let
        lastYear =
            Year (year - 1)

        scaleForYear =
            Scale.atDate ((toString year) ++ "-01-01") irppScale
    in
        Maybe.map3
            (\(EUR salaireIndividuForLastYear) (EUR salaireConjointForLastYear) (EUR salaireEnfant1ForLastYear) ->
                let
                    salaires =
                        EUR (salaireIndividuForLastYear + salaireConjointForLastYear + salaireEnfant1ForLastYear)
                in
                    Scale.compute salaires (\(EUR x) -> x) EUR scaleForYear
            )
            (salaireIndividu lastYear)
            (salaireConjoint lastYear)
            (salaireEnfant1 lastYear)


revenuDisponible : Year -> YearSerie EUR -> YearSerie EUR -> YearSerie EUR -> Maybe EUR
revenuDisponible year salaireIndividu salaireConjoint salaireEnfant1 =
    Maybe.map2
        (\(EUR irppForYear) (EUR allocationLogementForYear) ->
            EUR (irppForYear + allocationLogementForYear)
        )
        (irpp year salaireIndividu salaireConjoint salaireEnfant1)
        -- individu of menage is enfant1 of foyer fiscal, and has no conjoint!
        (allocationLogement year salaireEnfant1 (constantSerie (EUR 0)))



-- MODEL


type alias Model =
    { salairesIndividu : Dict Int EUR
    , salairesConjoint : Dict Int EUR
    , salairesEnfant1 : Dict Int EUR
    }


initialModel : Model
initialModel =
    { salairesIndividu =
        Dict.fromList
            [ ( 2014, EUR 40000 )
            , ( 2015, EUR 40000 )
            ]
    , salairesConjoint =
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

        salaireEnfant1 =
            salaireFromData model.salairesEnfant1

        scaleForYear =
            Scale.atDate "2015-01-01" irppScale

        allocationLogement2015 =
            -- individu of menage is enfant1 of foyer fiscal, and has no conjoint!
            allocationLogement (Year 2015) salaireEnfant1 (constantSerie (EUR 0))

        irpp2015 =
            irpp (Year 2015) salaireIndividu salaireConjoint salaireEnfant1

        revenuDisponible2015 =
            revenuDisponible (Year 2015) salaireIndividu salaireConjoint salaireEnfant1
    in
        div []
            [ p [] [ text ("Salaire par an individu : " ++ (toString model.salairesIndividu)) ]
            , p [] [ text ("Salaire par an conjoint : " ++ (toString model.salairesConjoint)) ]
            , p [] [ text ("Salaire par an enfant 1 : " ++ (toString model.salairesEnfant1)) ]
            , p [] [ Scale.view (\(EUR x) -> x) scaleForYear ]
            , p [] [ text ("irpp 2015 = " ++ (toString irpp2015)) ]
            , p [] [ text ("allocationLogement 2015 = " ++ (toString allocationLogement2015)) ]
            , p [] [ text ("revenuDisponible 2015 = " ++ (toString revenuDisponible2015)) ]
            ]
