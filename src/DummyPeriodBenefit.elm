module DummyPeriodBenefit exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Types exposing (..)


type EUR
    = EUR Float



-- FORMULAS


benefit : Month -> MonthSerie EUR -> Maybe EUR
benefit (Month month) salaire =
    Maybe.map3
        (\(EUR a) (EUR b) (EUR c) ->
            EUR ((a + b + c) * 0.7)
        )
        (salaire (Month (month - 1)))
        (salaire (Month (month - 2)))
        (salaire (Month (month - 3)))


income : Month -> MonthSerie EUR -> Maybe EUR
income month salaire =
    Maybe.map2
        (\(EUR salaire) (EUR benefit) -> EUR (salaire + benefit))
        (salaire month)
        (benefit month salaire)



-- MODEL


type alias Model =
    { salaires : Dict Int EUR
    }


initialModel : Model
initialModel =
    { salaires =
        Dict.fromList
            [ ( 1, EUR 1000 )
            , ( 2, EUR 1500 )
            , ( 3, EUR 1800 )
            , ( 4, EUR 2000 )
            ]
    }


salaireFromData : Dict Int EUR -> Month -> Maybe EUR
salaireFromData salaires =
    (\(Month month) -> Dict.get month salaires)



-- VIEW


view : Model -> Html msg
view model =
    let
        salaire =
            salaireFromData model.salaires
    in
        div []
            [ p [] [ text ("Salaire par mois : " ++ (toString model.salaires)) ]
            , p [] [ text ("salaire month_1 = " ++ (toString (salaire (Month 1)))) ]
            , p [] [ text ("benefit month_4 = " ++ (toString (benefit (Month 4) salaire))) ]
            , p [] [ text ("income month_4 = " ++ (toString (income (Month 4) salaire))) ]
            , p [] [ text ("salaire month_9 = " ++ (toString (salaire (Month 9)))) ]
            , p [] [ text ("benefit month_12 = " ++ (toString (benefit (Month 12) salaire))) ]
            , p [] [ text ("benefit month_6 = " ++ (toString (benefit (Month 6) salaire))) ]
            ]
