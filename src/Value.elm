module Value exposing (..)


type alias Currency =
    String


type Value
    = MonetaryAmount Currency Float
    | Amount Float


toFloat : Value -> Float
toFloat valueWithUnit =
    case valueWithUnit of
        MonetaryAmount _ value ->
            value

        Amount value ->
            value


toString : Value -> String
toString unit =
    case unit of
        MonetaryAmount _ float ->
            Basics.toString float

        Amount int ->
            Basics.toString int
