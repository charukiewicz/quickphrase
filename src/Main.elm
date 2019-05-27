module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Time


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gameMode : Maybe GameMode
    , gameLength : Maybe Int
    , gameActive : Bool
    , wordsList : Dict String (List String)
    }


type GameMode
    = TimedRound
    | TimedWord


init : () -> ( Model, Cmd Msg )
init _ =
    { gameMode = Nothing
    , gameLength = Nothing
    , gameActive = False
    , wordsList = Dict.empty
    }
        |> with Cmd.none


with : a -> b -> ( b, a )
with a b =
    ( b, a )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameActive of
        True ->
            Time.every 1000 (\_ -> NoOp)

        False ->
            Sub.none



-- UPDATE


type Msg
    = SetGameMode GameMode
    | SetGameLength String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGameMode gm ->
            { model
                | gameMode = Just gm
                , gameLength =
                    case gm of
                        TimedRound ->
                            Just 60

                        TimedWord ->
                            Just 10
            }
                |> with Cmd.none

        SetGameLength gl ->
            { model
                | gameLength =
                    String.toInt gl
                        |> (\newGl ->
                                case ( newGl, gl ) of
                                    ( Nothing, "" ) ->
                                        Nothing

                                    ( Nothing, _ ) ->
                                        model.gameLength

                                    ( Just _, _ ) ->
                                        newGl
                           )
            }
                |> with Cmd.none

        NoOp ->
            ( model, Cmd.none )



-- VIEW


layoutOptions : { options : List Option }
layoutOptions =
    { options = [] }


view : Model -> Browser.Document Msg
view model =
    { title = "FastPhrase"
    , body =
        [ Element.layoutWith
            layoutOptions
            [ Background.color (rgb255 23 35 60)
            , Font.color (rgb255 255 152 0)
            , Font.family [ Font.monospace ]
            ]
            (gameRender model)
        ]
    }


gameRender : Model -> Element Msg
gameRender model =
    case model.gameActive of
        True ->
            gameDisplay model

        False ->
            controlDisplay model


controlDisplay : Model -> Element Msg
controlDisplay model =
    column
        [ centerX
        , centerY
        , padding 10
        , spacing 10
        , Font.size 40
        ]
        [ row
            [ spacing 10
            , width fill
            ]
            [ renderBtn
                "Timed Round"
                (model.gameMode == Just TimedRound)
                (SetGameMode TimedRound)
            , renderBtn
                "Timed Word"
                (model.gameMode == Just TimedWord)
                (SetGameMode TimedWord)
            ]
        , row
            [ spacing 10, centerY ]
            [ Input.text
                [ Background.color (rgb255 23 35 60) ]
                { onChange = SetGameLength
                , text =
                    model.gameLength
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault ""
                , placeholder = Just <| Input.placeholder [] (text "Seconds")
                , label = Input.labelLeft [ centerY ] (text "Round Length")
                }
            ]
        , Input.button
            (btnStyle (model.gameMode /= Nothing))
            { label = text "Start Game"
            , onPress = Maybe.map (\_ -> NoOp) <| model.gameMode
            }
        ]


renderBtn : String -> Bool -> Msg -> Element Msg
renderBtn label isActive msg =
    Input.button
        (btnStyle isActive)
        { label = text label
        , onPress =
            if not <| isActive then
                Just msg

            else
                Nothing
        }


btnStyle : Bool -> List (Attribute msg)
btnStyle active =
    [ Border.solid
    , Border.color (rgb255 220 131 0)
    , Border.width 4
    , padding 20
    , centerX
    , width <| minimum 350 fill
    , Font.center
    ]
        ++ (if active then
                [ Background.color (rgb255 255 152 0)
                , Font.color (rgb255 23 35 60)
                ]

            else
                []
           )


gameDisplay : Model -> Element Msg
gameDisplay model =
    row
        [ centerX
        , centerY
        , Font.size 50
        ]
        [ text <| "Hello world" ]
