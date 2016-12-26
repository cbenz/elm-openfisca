module Types exposing (..)


type Rate
    = Rate Float



-- PERIODS


type Month
    = Month Int


type alias MonthSerie value =
    Month -> Maybe value


type Year
    = Year Int


type alias YearSerie value =
    Year -> Maybe value


type alias YearMultiSerie value =
    Year -> List value


constantSerie : value -> period -> Maybe value
constantSerie constant =
    always (Just constant)


constantMultiSerie : value -> period -> List value
constantMultiSerie constant =
    always [ constant ]
