module MonetaryAmount exposing (..)

import Types exposing (..)


type MonetaryAmount
    = MonetaryAmount Currency Float


map : (Float -> Float) -> MonetaryAmount -> MonetaryAmount
map f (MonetaryAmount currency x) =
    MonetaryAmount currency (f x)


map2 : (Float -> Float -> Float) -> MonetaryAmount -> MonetaryAmount -> MonetaryAmount
map2 f (MonetaryAmount currency x) (MonetaryAmount _ y) =
    MonetaryAmount currency (f x y)
