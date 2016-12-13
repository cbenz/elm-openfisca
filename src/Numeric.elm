module Numeric exposing (..)


linspace : Float -> Float -> Int -> List Float
linspace mini maxi num =
    let
        range =
            List.range 0 (num - 1)

        rangeMin =
            List.minimum range

        rangeMax =
            List.maximum range
    in
        List.filterMap
            (\x ->
                Maybe.map2
                    (\rangeMin rangeMax ->
                        let
                            xRatio =
                                (toFloat (x - rangeMin))
                                    / (toFloat (rangeMax - rangeMin))
                        in
                            (maxi - mini) * xRatio + mini
                    )
                    rangeMin
                    rangeMax
            )
            range
