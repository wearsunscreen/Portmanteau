module View exposing (view)

import Browser exposing (Document)
import Data exposing (getDefinition, getHint, getWord)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Seed, int, maxInt, minInt, step)
import String exposing (fromInt, repeat)
import Time exposing (posixToMillis)


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name isChecked =
    label
        [ style "padding" "20px"
        ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]


fromHumpty =
    "\"Well, 'slithy' means lithe and slimy'... You see, it's like a portmanteau - there are two meanings packed up into one word.\""


showHide : Bool -> String -> String
showHide b s =
    if b then
        s

    else
        repeat (String.length s) "*"


styleBase =
    [ style "position" "absolute"
    , style "font-size" "100%"
    , style "top" "50px"
    , style "right" "50px"
    , style "left" "100px"
    , style "text-align" "center"
    , style "background-color" "MediumTurquoise"
    , style "border-radius" "25px"
    ]


styleHint b =
    if b then
        [ style "background-color" "Bisque"
        , style "border-radius" "10px"
        ]

    else
        []


view : Model -> Document Msg
view model =
    case model.startTime of
        Nothing ->
            { title = "Portmanteau"
            , body = [ viewWelcome model ]
            }

        Just t ->
            { title = "Portmanteau"
            , body = [ viewStuff model ]
            }


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p styleBase
            [ h1 [] [ text "Portmanteau!" ]
            , div [ style "font-style" "italic" ]
                [ h2 [] [ text fromHumpty ]
                , h2 [] [ text "- Humpty Dumpty" ]
                , h2 [] [ text "- Though the Looking-Glass, 1871, Lewis Carroll" ]
                ]
            , button [ onClick CloseWelcomeScreen ] [ text "Ok" ]
            ]
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
                        |> fromInt

        time =
            case model.startTime of
                Nothing ->
                    "unknown"

                Just t ->
                    posixToMillis t |> fromInt
    in
    div styleBase
        [ fieldset [ style "border" "0" ]
            [ p [] []
            , checkbox ToggleHint1 "Show Hint 1" model.showHint1
            , checkbox ToggleHint2 "Show Hint 2" model.showHint2
            , checkbox TogglePortmanteau "Show Answer" model.showPortmanteau
            , button [ onClick NextQuestion ] [ text "Next" ]
            ]
        , p []
            [ h2 [] [ text "What is the portmanteau meaning: " ]
            ]
        , p []
            [ h1 [ style "background-color" "Khaki" ] [ text <| getDefinition model.question ]
            ]
        , p []
            [ h3 (styleHint model.showHint1) [ text <| showHide model.showHint1 <| getHint model.question 0 ]
            ]
        , p []
            [ h3 (styleHint model.showHint2) [ text <| showHide model.showHint2 <| getHint model.question 1 ]
            ]
        , p []
            [ h1
                [ style "background-color" "DarkSlate2"
                , style "font-size" "400%"
                ]
                [ text <| showHide model.showPortmanteau <| getWord model.question ]
            ]
        ]
