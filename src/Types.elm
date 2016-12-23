module Types exposing (..)


type Rate
    = Rate Float



-- PERIODS


type Month
    = Month Int


type alias MonthSerie a =
    Month -> a
