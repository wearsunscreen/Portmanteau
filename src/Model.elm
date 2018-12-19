module Model exposing (..)

import Maybe exposing (Maybe)
import Random exposing (Seed)
import Time exposing (Posix)


type alias Model =
    { randomSeed : Maybe Seed
    , startTime : Maybe Posix
    , question : Int
    , showHint1 : Bool
    , showHint2 : Bool
    , showPortmanteau : Bool
    }


type Msg
    = CloseWelcomeScreen
    | NextQuestion
    | StartApp Posix
    | ToggleHint1
    | ToggleHint2
    | TogglePortmanteau

{- handy utility function
-}
justOrDefault : Maybe a -> a -> a
justOrDefault maybe default =
    Maybe.withDefault default maybe