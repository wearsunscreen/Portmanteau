module View exposing (..)

import Data exposing (getDefinition, getHint, getWord)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Seed, int, maxInt, minInt, step)
import String exposing (repeat)


view : Model -> Html Msg
view model =
    case model.startTime of
        Nothing ->
            viewWelcome model

        Just t ->
            viewStuff model


fromHumpty =
    """\x0D\x0D
"Well, 'slithy' means lithe and slimy'... You see, it's like a portmanteau - there are two meanings packed up into one word."\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D\x0D
"""


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p aStyle
            [ h1 [] [ text "Portmanteau!" ]
            , p [] [ text fromHumpty ]
            , p [] [ text "- Humpty Dumpty" ]
            , p [] [ text "- Though the Looking-Glass, 1871, Lewis Carroll" ]
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
                        |> toString

        time =
            case model.startTime of
                Nothing ->
                    "unknown"

                Just t ->
                    toString t
    in
        div aStyle
            [ fieldset []
                [ checkbox ToggleHint1 "Show Hint 1" model.showHint1
                , checkbox ToggleHint2 "Show Hint 2" model.showHint2
                , checkbox TogglePortmanteau "Show Answer" model.showPortmanteau
                , button [ onClick NextQuestion ] [ text "Next" ]
                ]
            , p []
                [ h2 [] [ text "What is the portmanteau meaning: " ]
                ]
            , p []
                [ h1 [] [ text <| getDefinition model.question ]
                ]
            , p []
                [ h3 [] [ text <| showHide model.showHint1 <| getHint model.question 0 ]
                ]
            , p []
                [ h3 [] [ text <| showHide model.showHint2 <| getHint model.question 1 ]
                ]
            , p []
                [ h1 [] [ text <| showHide model.showPortmanteau <| getWord model.question ]
                ]
            ]


showHide : Bool -> String -> String
showHide b s =
    if b then
        s
    else
        repeat (String.length s) "*"


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name isChecked =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]


aStyle =
    [ style
        [ ( "position", "absolute" )
        , ( "font-size", "100%" )
        , ( "top", "50px" )
        , ( "left", "100px" )
        ]
    ]
