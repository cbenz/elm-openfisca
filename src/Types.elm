module Types exposing (..)


type Rate
    = Rate Float



-- PERIODS


type Month
    = Month Int


type alias MonthSerie a =
    Month -> Maybe a


type Year
    = Year Int


type alias YearSerie a =
    Year -> Maybe a


constantSerie : value -> a -> Maybe value
constantSerie constant =
    always (Just constant)
