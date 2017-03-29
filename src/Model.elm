module Model exposing (..)

import Maybe exposing (Maybe)
import Random exposing (Seed)
import Time exposing (Time)


type alias Model =
    { randomSeed : Maybe Seed
    , startTime : Maybe Time
    , question : Int
    }


type Msg
    = CloseWelcomeScreen
    | StartApp Time


(??) : Maybe a -> a -> a
(??) maybe default =
    case maybe of
        Just x ->
            x

        Nothing ->
            default
