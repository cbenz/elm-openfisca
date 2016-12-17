module Scale exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Value exposing (Value(..))


type alias Bracket =
    { thresholds : List ( Date, Date, Float )
    , rates : List ( Date, Date, Rate )
    }


type alias Date =
    String


type alias Rate =
    Float


type alias Scale =
    List ( Value, Rate )


type alias ScaleWithDate =
    List
        { thresholds : List ( Date, Date, Value )
        , rates : List ( Date, Date, Rate )
        }



-- BUILD VALUES


scale : (number -> Value) -> List ( number, Rate ) -> Scale
scale thresholdTagger brackets =
    List.map
        (\( threshold, rate ) -> ( thresholdTagger threshold, rate ))
        brackets


{-| Build a `ScaleWithDate` from a `List Bracket`.

Example:
    scaleWithDate
        (MonetaryAmount "€")
        [ { thresholds =
                [ ( "2014-01-01", "2015-12-31", 0 )
                ]
          , rates =
                [ ( "2014-01-01", "2015-12-31", 0 )
                ]
          }
        ]
-}
scaleWithDate : (Float -> Value) -> List Bracket -> ScaleWithDate
scaleWithDate thresholdTagger brackets =
    List.map
        (\{ thresholds, rates } ->
            { thresholds =
                List.map
                    (\( start, stop, threshold ) -> ( start, stop, thresholdTagger threshold ))
                    thresholds
            , rates = rates
            }
        )
        brackets



-- COMPUTE


compute : Float -> Scale -> Float
compute inputValue scale =
    let
        matchingBrackets =
            scale
                |> List.reverse
                |> List.filter (\( threshold, _ ) -> inputValue > (Value.toFloat threshold))
    in
        List.map2
            (\( threshold, rate ) previousThreshold ->
                let
                    thresholdValue =
                        Value.toFloat threshold
                in
                    case previousThreshold of
                        Nothing ->
                            (inputValue - thresholdValue) * rate

                        Just previousThreshold ->
                            ((Value.toFloat previousThreshold) - thresholdValue) * rate
            )
            matchingBrackets
            (Nothing
                :: (List.map (\( threshold, _ ) -> Just threshold)
                        (List.take ((List.length matchingBrackets) - 1) matchingBrackets)
                   )
            )
            |> List.sum


atDate : Date -> ScaleWithDate -> Scale
atDate date scale =
    let
        findForDate xs =
            List.filterMap
                (\( start, stop, x ) ->
                    if start <= date && date <= stop then
                        Just x
                    else
                        Nothing
                )
                xs
                |> List.head
    in
        List.filterMap
            (\{ thresholds, rates } ->
                Maybe.map2 (,) (findForDate thresholds) (findForDate rates)
            )
            scale



-- VIEW


view : Scale -> Html msg
view scale =
    let
        tableWithBorders : List (List (Html msg)) -> Html msg
        tableWithBorders rows =
            table
                [ style
                    [ ( "border", "1px solid" )
                    , ( "border-collapse", "collapse" )
                    ]
                ]
                [ tbody []
                    (rows
                        |> List.map
                            (\cells ->
                                tr []
                                    (cells
                                        |> List.map
                                            (\cell ->
                                                td
                                                    [ style
                                                        [ ( "border", "1px solid" )
                                                        , ( "padding", "1em" )
                                                        ]
                                                    ]
                                                    [ cell ]
                                            )
                                    )
                            )
                    )
                ]
    in
        div []
            [ tableWithBorders
                (List.map2
                    (\( threshold, rate ) nextThreshold ->
                        [ let
                            toThreshold threshold =
                                "jusqu'à " ++ (Value.toString threshold)
                          in
                            case ( threshold, nextThreshold ) of
                                ( MonetaryAmount _ 0, Just nextThreshold ) ->
                                    text (toThreshold nextThreshold)

                                ( Amount 0, Just nextThreshold ) ->
                                    text (toThreshold nextThreshold)

                                ( threshold, Just nextThreshold ) ->
                                    text ("de " ++ (Value.toString threshold) ++ " à " ++ (Value.toString nextThreshold))

                                ( threshold, Nothing ) ->
                                    text ("supérieur à " ++ (Value.toString threshold))
                        , text (toString rate)
                        ]
                    )
                    scale
                    ((List.drop 1 scale
                        |> List.map (\( threshold, _ ) -> Just threshold)
                     )
                        ++ [ Nothing ]
                    )
                )
            ]
