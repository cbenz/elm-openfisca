module Main exposing (..)

import DummyTaxBenefitSystem
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
    { dummyHouseholdTax : DummyTaxBenefitSystem.Model
    , dummyPeriodBenefit : DummyPeriodBenefit.Model
    , senegal : Senegal.Model
    }


initialModel : Model
initialModel =
    { dummyHouseholdTax = DummyTaxBenefitSystem.initialModel
    , dummyPeriodBenefit = DummyPeriodBenefit.initialModel
    , senegal = Senegal.initialModel
    }


type Msg
    = DummyHouseholdTaxMsg DummyTaxBenefitSystem.Msg
    | SenegalMsg Senegal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DummyHouseholdTaxMsg innerMsg ->
            let
                ( newChildModel, newChildCmd ) =
                    DummyTaxBenefitSystem.update innerMsg model.dummyHouseholdTax
            in
                ( { model | dummyHouseholdTax = newChildModel }
                , Cmd.map DummyHouseholdTaxMsg newChildCmd
                )

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
            [ text "Source code: "
            , let
                url =
                    "https://github.com/cbenz/elm-openfisca"
              in
                a [ href url ] [ text url ]
            ]
        , hr [] []
        , section []
            [ h1 [] [ text "Dummy Household Tax" ]
            , p [] [ text "To test periods, (individuals / groups / relationships), scales and plots." ]
            , DummyTaxBenefitSystem.view model.dummyHouseholdTax
                |> Html.map DummyHouseholdTaxMsg
            ]
        , hr [] []
        , section []
            [ h1 [] [ text "Dummy Benefit" ]
            , p [] [ text "To test periods." ]
            , DummyPeriodBenefit.view DummyPeriodBenefit.initialModel
            ]
        , hr [] []
        , section []
            [ h1 [] [ text "Sénégal" ]
            , Senegal.view model.senegal
                |> Html.map SenegalMsg
            ]
        ]
