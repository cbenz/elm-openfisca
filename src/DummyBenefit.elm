module DummyBenefit exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Types exposing (..)


type EUR
    = EUR Float



-- FORMULAS


benefit : Month -> MonthSerie EUR -> EUR
benefit (Month month) salaire =
    (List.range 1 3
        |> List.map (\i -> salaire (Month (month - i)))
        |> List.map (\(EUR f) -> f)
        |> List.sum
    )
        * 0.7
        |> EUR


income : Month -> MonthSerie EUR -> EUR
income month salaire =
    let
        (EUR salaireM) =
            salaire month

        (EUR benefitM) =
            benefit month salaire
    in
        EUR (salaireM + benefitM)



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


salaireFromData : Dict Int EUR -> Month -> EUR
salaireFromData salaires =
    (\(Month month) ->
        Dict.get month salaires
            |> Maybe.withDefault (EUR 0)
     -- TODO Handle Nothing in caller
    )



-- VIEW


view : Model -> Html msg
view model =
    let
        salaire =
            salaireFromData model.salaires
    in
        div []
            [ p [] [ text ("Salaire par mois : " ++ (toString model.salaires)) ]
            , p []
                [ text ("benefit month_4 = " ++ (toString (benefit (Month 4) salaire))) ]
            , p []
                [ text ("income month_4 = " ++ (toString (income (Month 4) salaire))) ]
            ]
