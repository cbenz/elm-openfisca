module DummyHouseholdTax exposing (..)

import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    { salaires : Dict Int EUR }


type IndividualsGroup
    = FoyerFiscal String
    | Menage String


type alias Relationships =
    EveryDict IndividualsGroup (List String)


type alias Model =
    { individuals : Dict String Individual
    , relationships : Relationships
    }


initialModel : Model
initialModel =
    { individuals =
        Dict.fromList
            [ ( "individualA"
              , { salaires =
                    Dict.fromList
                        [ ( 2014, EUR 40000 )
                        , ( 2015, EUR 40000 )
                        ]
                }
              )
            , ( "individualB"
              , { salaires =
                    Dict.fromList
                        [ ( 2014, EUR 10000 )
                        , ( 2015, EUR 15000 )
                        ]
                }
              )
            , ( "individualC"
              , { salaires =
                    Dict.fromList
                        [ ( 2014, EUR 20000 )
                        , ( 2015, EUR 15000 )
                        ]
                }
              )
            ]
    , relationships =
        EveryDict.fromList
            [ ( FoyerFiscal "1", [ "individualA", "individualB", "individualC" ] )
            , ( Menage "1", [ "individualA", "individualB" ] )
            , ( Menage "2", [ "individualC" ] )
            ]
    }



-- UPDATE


type Msg
    = AddIndividual
    | RemoveIndividual String
    | SetIndividualName String String
    | SetIndividualSalaire Year String EUR
    | SetRelationship String IndividualsGroup Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddIndividual ->
            let
                newIndividuals =
                    Dict.insert "new"
                        { salaires =
                            Dict.fromList
                                [ ( 2014, EUR 0 )
                                , ( 2015, EUR 0 )
                                ]
                        }
                        model.individuals

                newModel =
                    { model | individuals = newIndividuals }
            in
                ( newModel, Cmd.none )

        RemoveIndividual name ->
            let
                newIndividuals =
                    Dict.remove name model.individuals

                newRelationships =
                    EveryDict.map
                        (\individualsGroup names -> List.filter ((/=) name) names)
                        model.relationships

                newModel =
                    { model
                        | individuals = newIndividuals
                        , relationships = newRelationships
                    }
            in
                ( newModel, Cmd.none )

        SetIndividualName oldName newName ->
            let
                newIndividuals =
                    case Dict.get oldName model.individuals of
                        Nothing ->
                            model.individuals

                        Just individual ->
                            model.individuals
                                |> Dict.remove oldName
                                |> Dict.insert newName individual

                newModel =
                    { model | individuals = newIndividuals }
            in
                ( newModel, Cmd.none )

        SetIndividualSalaire (Year year) name salaire ->
            let
                newIndividuals =
                    Dict.update name
                        (Maybe.map
                            (\individual ->
                                let
                                    newSalaires =
                                        Dict.insert year salaire individual.salaires
                                in
                                    { individual | salaires = newSalaires }
                            )
                        )
                        model.individuals

                newModel =
                    { model | individuals = newIndividuals }
            in
                ( newModel, Cmd.none )

        SetRelationship name individualsGroup checked ->
            let
                newRelationships =
                    EveryDict.update individualsGroup
                        (Maybe.map
                            (\names ->
                                if checked then
                                    names ++ [ name ]
                                else
                                    List.filter ((/=) name) names
                            )
                        )
                        model.relationships

                newModel =
                    { model | relationships = newRelationships }
            in
                ( newModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
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

                Just names ->
                    names
                        |> List.filterMap
                            (\name ->
                                Dict.get name model.individuals
                                    |> Maybe.andThen (.salaires >> Dict.get year)
                            )

        allocationLogement2015Menage1 : EUR
        allocationLogement2015Menage1 =
            allocationLogement year2015 (salaires (Menage "1"))

        allocationLogement2015Menage2 : EUR
        allocationLogement2015Menage2 =
            -- TODO Handle menage with no individu
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
            [ viewIndividuals model.individuals
            , viewRelationships (Dict.keys model.individuals) model.relationships
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


viewIndividual : String -> Individual -> Html Msg
viewIndividual name individual =
    div []
        [ label []
            [ text "name "
            , input
                [ onInput (SetIndividualName name)
                , value name
                ]
                []
            , button [ onClick (RemoveIndividual name) ] [ text "Remove" ]
            ]
        , br [] []
        , label []
            [ text "salaires"
            , ul []
                (individual.salaires
                    |> Dict.toList
                    |> List.map
                        (\( year, EUR salaire ) ->
                            li []
                                [ text ("Year " ++ (toString year) ++ " ")
                                , input
                                    [ Html.Attributes.min "0"
                                    , onInput
                                        (\str ->
                                            SetIndividualSalaire (Year year)
                                                name
                                                (case String.toFloat str of
                                                    Ok float ->
                                                        EUR float

                                                    Err _ ->
                                                        EUR salaire
                                                )
                                        )
                                    , type_ "number"
                                    , value (toString salaire)
                                    ]
                                    []
                                , text " EUR"
                                ]
                        )
                )
            ]
        ]


viewIndividuals : Dict String Individual -> Html Msg
viewIndividuals individuals =
    div []
        [ ul []
            (individuals
                |> Dict.toList
                |> List.map
                    (\( name, salaires ) ->
                        li []
                            [ viewIndividual name salaires ]
                    )
            )
        , button [ onClick AddIndividual ] [ text "Add individual" ]
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


viewRelationships : List String -> Relationships -> Html Msg
viewRelationships individualNames relationships =
    ul []
        (relationships
            |> EveryDict.toList
            |> List.map
                (\( individualsGroup, names ) ->
                    li []
                        ((text (toString individualsGroup))
                            :: (List.map
                                    (\name ->
                                        label []
                                            [ input
                                                [ checked (List.member name names)
                                                , onCheck (SetRelationship name individualsGroup)
                                                , type_ "checkbox"
                                                ]
                                                []
                                            , text name
                                            ]
                                    )
                                    individualNames
                               )
                        )
                )
        )
