module View exposing (..)

import Data exposing (getDefinition, getWord)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Seed, int, maxInt, minInt, step)


view : Model -> Html Msg
view model =
    case model.startTime of
        Nothing ->
            viewWelcome model

        Just t ->
            viewStuff model


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome!" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Ok" ]
        ]


viewStuff : Model -> Html Msg
viewStuff model =
    let
        seed =
            case model.randomSeed of
                Nothing ->
                    "unknown"

                Just s ->
                    Random.step (int minInt maxInt) s
                        |> Tuple.first
                        |> toString

        time =
            case model.startTime of
                Nothing ->
                    "unknown"

                Just t ->
                    toString t
    in
        div []
            [ p []
                [ h1 [] [ text "What is the portmanteau meaning: " ]
                ]
            , p []
                [ h1 [] [ text <| Data.getDefinition model.question ?? (" error: malformed data at index " ++ (toString model.question)) ]
                ]
            , p []
                [ h2 [] [ text <| Data.getHint model.question 0 ?? (" error: malformed data at index " ++ (toString model.question)) ]
                ]
            , p []
                [ h2 [] [ text <| Data.getHint model.question 1 ?? (" error: malformed data at index " ++ (toString model.question)) ]
                ]
            , p []
                [ h1 [] [ text <| Data.getWord model.question ?? " error: bad index " ]
                ]
            ]
