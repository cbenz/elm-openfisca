module Types exposing (..)


type alias Currency =
    String


type alias Date =
    String


type alias Period =
    String


type alias Rate =
    Float


type alias Scale =
    List ( ValueWithUnit, Rate )


type alias TimeChangingScale =
    List
        { thresholds : List ( Date, Date, ValueWithUnit )
        , rates : List ( Date, Date, Rate )
        }


type ValueWithUnit
    = MonetaryAmount Currency Float
    | Amount Float
