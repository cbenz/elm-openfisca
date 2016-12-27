module DummyTaxBenefitSystem exposing (..)

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
        |> sumEURList
        |> mapEUR
            (\salairesSum ->
                if salairesSum < 30000 then
                    2000
                else
                    0
            )


irpp : Year -> YearMultiSerie EUR -> EUR
irpp ((Year yearInt) as year) salairesFoyerFiscal =
    salairesFoyerFiscal (Year (yearInt - 1))
        |> sumEURList
        |> (\salairesSum ->
                Scale.atYearStart year irppScale
                    |> Scale.compute salairesSum (\(EUR x) -> x) EUR
           )


revenuDisponible : Year -> YearMultiSerie EUR -> YearMultiSerie EUR -> EUR
revenuDisponible year salairesMenage salairesFoyerFiscal =
    map2EUR (+)
        (irpp year salairesFoyerFiscal)
        (allocationLogement year salairesMenage)



-- MODEL


type EUR
    = EUR Float


mapEUR : (Float -> Float) -> EUR -> EUR
mapEUR f (EUR x) =
    EUR (f x)


map2EUR : (Float -> Float -> Float) -> EUR -> EUR -> EUR
map2EUR f (EUR x) (EUR y) =
    EUR (f x y)


sumEURList : List EUR -> EUR
sumEURList =
    (List.map (\(EUR x) -> x)) >> List.sum >> EUR


type alias InputValues =
    { salaires : Dict Int EUR }


type alias Individual =
    ( String, InputValues )


type IndividualsGroup
    = FoyerFiscal String
    | Menage String


type alias Relationships =
    EveryDict IndividualsGroup (List String)


type alias Model =
    { individuals : List Individual
    , relationships : Relationships
    }



-- TODO Handle menage with no individu (ensure series don't return empty lists leading to (EUR 0) when summing, but return a Maybe)
-- TODO Ensure that an individual belongs to one and only one entity before computing anything


initialModel : Model
initialModel =
    { individuals =
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
    | RemoveIndividual Int
    | SetIndividualName Int String
    | SetIndividualSalaire Year Int EUR
    | SetRelationship String IndividualsGroup Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nameAtIndex : Int -> Maybe String
        nameAtIndex index =
            model.individuals
                |> List.indexedMap
                    (\index1 ( name, _ ) ->
                        if index == index1 then
                            Just name
                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.head
    in
        case msg of
            AddIndividual ->
                let
                    newIndividuals =
                        model.individuals
                            ++ [ ( "new"
                                 , { salaires =
                                        Dict.fromList
                                            [ ( 2014, EUR 0 )
                                            , ( 2015, EUR 0 )
                                            ]
                                   }
                                 )
                               ]

                    newModel =
                        { model | individuals = newIndividuals }
                in
                    ( newModel, Cmd.none )

            RemoveIndividual index ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.indexedMap
                                (\index1 individual ->
                                    if index == index1 then
                                        Nothing
                                    else
                                        Just individual
                                )
                            |> List.filterMap identity

                    newRelationships =
                        case nameAtIndex index of
                            Nothing ->
                                model.relationships

                            Just name ->
                                model.relationships
                                    |> EveryDict.map (\_ names -> List.filter ((/=) name) names)

                    newModel =
                        { model
                            | individuals = newIndividuals
                            , relationships = newRelationships
                        }
                in
                    ( newModel, Cmd.none )

            SetIndividualName index newName ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.indexedMap
                                (\index1 ( name, values ) ->
                                    ( if index == index1 then
                                        newName
                                      else
                                        name
                                    , values
                                    )
                                )

                    newRelationships =
                        case nameAtIndex index of
                            Nothing ->
                                model.relationships

                            Just oldName ->
                                model.relationships
                                    |> EveryDict.map
                                        (\_ names ->
                                            names
                                                |> List.map
                                                    (\name ->
                                                        if name == oldName then
                                                            newName
                                                        else
                                                            name
                                                    )
                                        )

                    newModel =
                        { model
                            | individuals = newIndividuals
                            , relationships = newRelationships
                        }
                in
                    ( newModel, Cmd.none )

            SetIndividualSalaire (Year year) index salaire ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.indexedMap
                                (\index1 ( name, values ) ->
                                    ( name
                                    , if index == index1 then
                                        let
                                            newSalaires =
                                                Dict.insert year salaire values.salaires
                                        in
                                            { values | salaires = newSalaires }
                                      else
                                        values
                                    )
                                )

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
                                model.individuals
                                    |> Dict.fromList
                                    |> Dict.get name
                                    |> Maybe.andThen (.salaires >> Dict.get year)
                            )

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
            [ viewIndividuals model.individuals
            , viewRelationships (List.map Tuple.first model.individuals) model.relationships
            , p [] [ text "The scale used by irpp:" ]
            , Scale.view (\(EUR x) -> x) scaleForYear
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
            , p [] [ text "Plot the variation of the salary on X axis:" ]
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


viewIndividual : Int -> Individual -> Html Msg
viewIndividual index ( name, individual ) =
    div []
        [ label []
            [ text "name "
            , input
                [ onInput (SetIndividualName index)
                , value name
                ]
                []
            , button [ onClick (RemoveIndividual index) ] [ text "Remove" ]
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
                                                index
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


viewIndividuals : List Individual -> Html Msg
viewIndividuals individuals =
    fieldset []
        [ legend [] [ text "Individuals" ]
        , div []
            [ ul []
                (individuals
                    |> List.indexedMap
                        (\index individual ->
                            li []
                                [ viewIndividual index individual ]
                        )
                )
            , button [ onClick AddIndividual ] [ text "Add individual" ]
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


viewRelationships : List String -> Relationships -> Html Msg
viewRelationships individualNames relationships =
    fieldset []
        [ legend [] [ text "Relationships" ]
        , p [] [ text "Associate individuals to groups:" ]
        , ul []
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
        ]
