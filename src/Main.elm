module Main exposing (..)

import DummyHouseholdTax
import DummyPeriodBenefit
import Html exposing (..)
import Html.Attributes exposing (..)
import Senegal


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { senegal : Senegal.Model
    }


initialModel : Model
initialModel =
    { senegal = Senegal.initialModel }


type Msg
    = SenegalMsg Senegal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SenegalMsg innerMsg ->
            let
                ( newChildModel, newChildCmd ) =
                    Senegal.update innerMsg model.senegal
            in
                ( { model | senegal = newChildModel }
                , Cmd.map SenegalMsg newChildCmd
                )


view : Model -> Html Msg
view model =
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
        , section []
            [ h1 [] [ text "Dummy Benefit" ]
            , DummyPeriodBenefit.view DummyPeriodBenefit.initialModel
            ]
        , hr [] []
        , section []
            [ h1 [] [ text "Dummy Household Tax" ]
            , DummyHouseholdTax.view DummyHouseholdTax.initialModel
            ]
        , hr [] []
        , section []
            [ h1 [] [ text "Sénégal" ]
            , Html.map SenegalMsg (Senegal.view model.senegal)
            ]
        ]
