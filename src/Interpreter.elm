module Interpreter exposing (..)

import Types exposing (..)


interpretScale : Scale -> Float -> Float
interpretScale scale value =
    let
        matchingBrackets =
            scale
                |> List.reverse
                |> List.filter (\( threshold, _ ) -> value > (Types.value threshold))
    in
        List.map2
            (\( threshold, rate ) previousThreshold ->
                let
                    thresholdValue =
                        Types.value threshold
                in
                    case previousThreshold of
                        Nothing ->
                            (value - thresholdValue) * rate

                        Just previousThreshold ->
                            ((Types.value previousThreshold) - thresholdValue) * rate
            )
            matchingBrackets
            (Nothing
                :: (List.map (\( threshold, _ ) -> Just threshold)
                        (List.take ((List.length matchingBrackets) - 1) matchingBrackets)
                   )
            )
            |> List.sum
