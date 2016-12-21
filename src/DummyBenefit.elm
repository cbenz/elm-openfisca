module DummyBenefit exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Types exposing (Currency, MonetaryAmount(..))


-- CONSTANTS


currency : Currency
currency =
    "€"



-- FORMULAS


type alias Month =
    Int


benefit : Month -> (Month -> MonetaryAmount) -> MonetaryAmount
benefit month salaire =
    let
        toFloat (MonetaryAmount _ f) =
            f
    in
        (List.range 1 3
            |> List.map
                (\n ->
                    salaire (month - n)
                        |> toFloat
                )
            |> List.sum
        )
            * 0.7
            |> MonetaryAmount currency


income : Month -> (Month -> MonetaryAmount) -> MonetaryAmount
income month salaire =
    let
        toFloat (MonetaryAmount _ f) =
            f
    in
        (salaire month |> toFloat)
            + (benefit month salaire |> toFloat)
            |> MonetaryAmount currency



-- MODEL


type alias Model =
    { salaires : Dict Month MonetaryAmount
    }


initialModel : Model
initialModel =
    { salaires =
        Dict.fromList
            [ ( 1, MonetaryAmount currency 1000 )
            , ( 2, MonetaryAmount currency 1500 )
            , ( 3, MonetaryAmount currency 1800 )
            , ( 4, MonetaryAmount currency 2000 )
            ]
    }


salaireFromData : Dict Month MonetaryAmount -> Month -> MonetaryAmount
salaireFromData salaires =
    (\month ->
        Dict.get month salaires
            |> Maybe.withDefault (MonetaryAmount currency 0)
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
            [ p [] [ text ("Salaires par mois: " ++ (toString model.salaires)) ]
            , p []
                [ text ("benefit month_4 = " ++ (toString (benefit 4 salaire))) ]
            , p []
                [ text ("income month_4 = " ++ (toString (income 4 salaire))) ]
            ]
