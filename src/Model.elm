module Model exposing (..)

import Maybe exposing (Maybe)
import Random exposing (Seed)
import Time exposing (Time)


type alias Model =
    { randomSeed : Maybe Seed
    , startTime : Maybe Time
    , question : Int
    , showHint1 : Bool
    , showHint2 : Bool
    , showPortmanteau : Bool
    }


type Msg
    = CloseWelcomeScreen
    | NextQuestion
    | StartApp Time
    | ToggleHint1
    | ToggleHint2
    | TogglePortmanteau


(??) : Maybe a -> a -> a
(??) maybe default =
    case maybe of
        Just x ->
            x

        Nothing ->
            default
