module Types exposing (..)


type alias Currency =
    String


type MonetaryAmount
    = MonetaryAmount Currency Float


type Rate
    = Rate Float
