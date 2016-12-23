module Scale exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


type alias Bracket value =
    { thresholds : List ( Date, Date, value )
    , rates : List ( Date, Date, Rate )
    }


type alias Date =
    String


type alias Scale value =
    List ( value, Rate )


type alias ScaleWithDates value =
    List (Bracket value)



-- BUILD VALUES


scale : (number -> value) -> List ( number, Float ) -> Scale value
scale thresholdTagger brackets =
    List.map
        (\( threshold, rate ) -> ( thresholdTagger threshold, Rate rate ))
        brackets


{-| Build a `ScaleWithDates`.

Example:
    scaleWithDates
        Euro
        [ { thresholds =
                [ ( "2014-01-01", "2015-12-31", 0 )
                ]
          , rates =
                [ ( "2014-01-01", "2015-12-31", 0 )
                ]
          }
        ]
-}
scaleWithDates :
    (Float -> value)
    -> List
        { thresholds : List ( Date, Date, Float )
        , rates : List ( Date, Date, Float )
        }
    -> ScaleWithDates value
scaleWithDates thresholdTagger brackets =
    List.map
        (\{ thresholds, rates } ->
            { thresholds =
                List.map
                    (\( start, stop, threshold ) -> ( start, stop, thresholdTagger threshold ))
                    thresholds
            , rates =
                List.map
                    (\( start, stop, threshold ) -> ( start, stop, Rate threshold ))
                    rates
            }
        )
        brackets



-- COMPUTE


compute : value -> (value -> Float) -> (Float -> value) -> Scale value -> value
compute inputValue valueToFloat tagger scale =
    let
        matchingBrackets =
            scale
                |> List.reverse
                |> List.filter (\( threshold, _ ) -> valueToFloat inputValue > valueToFloat threshold)
    in
        List.map2
            (\( threshold, Rate rate ) previousThreshold ->
                let
                    thresholdValue =
                        valueToFloat threshold
                in
                    case previousThreshold of
                        Nothing ->
                            (valueToFloat inputValue - thresholdValue) * rate

                        Just previousThreshold ->
                            ((valueToFloat previousThreshold) - thresholdValue) * rate
            )
            matchingBrackets
            (Nothing
                :: (List.map (\( threshold, _ ) -> Just threshold)
                        (List.take ((List.length matchingBrackets) - 1) matchingBrackets)
                   )
            )
            |> List.sum
            |> tagger


atDate : Date -> ScaleWithDates value -> Scale value
atDate date scale =
    -- TODO Return Maybe (Scale value) if date returns no bracket?
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


view : (value -> Float) -> Scale value -> Html msg
view valueToFloat scale =
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

        nextThresholds =
            (scale
                |> List.drop 1
                |> List.map (\( threshold, _ ) -> Just threshold)
            )
                ++ [ Nothing ]
    in
        div []
            [ tableWithBorders
                (List.map2
                    (\( threshold, Rate rate ) nextThreshold ->
                        [ case ( valueToFloat threshold, nextThreshold ) of
                            ( 0, Just nextThreshold ) ->
                                text ("jusqu'à " ++ (toString nextThreshold))

                            ( _, Just nextThreshold ) ->
                                text ("de " ++ (toString threshold) ++ " à " ++ (toString nextThreshold))

                            ( _, Nothing ) ->
                                text ("supérieur à " ++ (toString threshold))
                        , text ((toString (rate * 100)) ++ " %")
                        ]
                    )
                    scale
                    nextThresholds
                )
            ]
