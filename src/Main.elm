module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode
import Random
import Task
import Time
import WordList


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
    , gameTimer : Maybe Int
    , wordList : Dict String (List String)
    , wordCategorySelect : Bool
    , selectedCategories : List String
    , seenWords : List String
    , randGen : Maybe (Random.Generator Int)
    }


type GameMode
    = TimedRound
    | TimedWord


init : () -> ( Model, Cmd Msg )
init _ =
    { gameMode = Nothing
    , gameLength = Nothing
    , gameActive = False
    , gameTimer = Nothing
    , wordList = WordList.wordList
    , wordCategorySelect = False
    , selectedCategories = []
    , seenWords = []
    , randGen = Nothing
    }
        |> with Cmd.none


with : a -> b -> ( b, a )
with a b =
    ( b, a )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameTimer of
        Just time ->
            Time.every 1000 (\_ -> Tick time)

        Nothing ->
            Sub.none



-- UPDATE


type Msg
    = SetGameMode GameMode
    | SetGameLength String
    | ToggleCategorySelection Bool
    | ToggleCategory String
    | InitializeGameState
    | StartGame
    | NextRound
    | NextRoundWithRand Int
    | Tick Int
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

        ToggleCategorySelection isOpen ->
            { model | wordCategorySelect = isOpen }
                |> with Cmd.none

        ToggleCategory category ->
            { model
                | selectedCategories =
                    toggleListElement model.selectedCategories category
            }
                |> with Cmd.none

        InitializeGameState ->
            let
                words =
                    model.selectedCategories
                        |> List.map (\cat -> Dict.get cat model.wordList)
                        |> List.filterMap identity
                        |> List.concat
            in
            { model
                | randGen = Just <| Random.int 1 <| List.length words
            }
                |> with (Task.perform identity <| Task.succeed StartGame)

        StartGame ->
            { model
                | gameActive = True
                , gameTimer = model.gameLength
            }
                |> with Cmd.none

        NextRound ->
            let
                randCmd =
                    case model.randGen of
                        Just generator ->
                            Random.generate NextRoundWithRand generator

                        Nothing ->
                            Cmd.none
            in
            model
                |> with randCmd

        NextRoundWithRand int ->
            let
                word =
                    model.selectedCategories
                        |> List.map (\cat -> Dict.get cat model.wordList)
                        |> List.filterMap identity
                        |> List.concat
                        |> List.drop (int - 1)
                        |> List.head
                        |> Maybe.withDefault ""
            in
            { model
                | gameTimer = Nothing
                , seenWords = word :: model.seenWords
            }
                |> with (Task.perform identity <| Task.succeed StartGame)

        Tick time ->
            let
                newGameTimer =
                    model.gameTimer
                        |> Maybe.map (\x -> x - 1)
            in
            (case newGameTimer of
                Just 0 ->
                    { model
                        | gameTimer = Nothing
                        , gameActive = False
                    }

                Just _ ->
                    { model | gameTimer = newGameTimer }

                Nothing ->
                    { model
                        | gameTimer = Nothing
                        , gameActive = False
                    }
            )
                |> with Cmd.none

        NoOp ->
            ( model, Cmd.none )


toggleListElement : List a -> a -> List a
toggleListElement list elem =
    if List.member elem list then
        List.filter (\x -> x /= elem) list

    else
        list ++ [ elem ]



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
            ([ Background.color (rgb255 23 35 60)
             , Font.color (rgb255 255 152 0)
             , Font.family [ Font.monospace ]
             ]
                ++ (if model.wordCategorySelect then
                        [ onClick <| ToggleCategorySelection False ]

                    else if model.gameActive then
                        [ onClick NextRound ]

                    else
                        []
                   )
            )
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
        [ width <| maximum 800 fill
        , centerX
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
                [ Background.color (rgb255 23 35 60)
                , Border.color (rgb255 220 131 0)
                , Border.width 4
                ]
                { onChange = SetGameLength
                , text =
                    model.gameLength
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault ""
                , placeholder = Just <| Input.placeholder [] (text "Seconds")
                , label = Input.labelLeft [ centerY ] (text "Round Length")
                }
            ]
        , wrappedRow [ spacing 10, centerY, width fill ]
            [ text "Categories"
            , categoryControl model
            ]
        , startGameBtn model
        ]


startGameBtn : Model -> Element Msg
startGameBtn model =
    let
        readyToStart =
            model.gameMode /= Nothing && model.gameLength /= Nothing && (not <| List.isEmpty model.selectedCategories)
    in
    Input.button
        (btnStyle readyToStart)
        { label = text "Start Game"
        , onPress =
            if readyToStart then
                Maybe.map (\_ -> InitializeGameState) <| model.gameMode

            else
                Nothing
        }


categoryControl : Model -> Element Msg
categoryControl model =
    el
        ([ width fill
         ]
            ++ (if not model.wordCategorySelect then
                    [ Border.color (rgb255 220 131 0)
                    , Border.width 4
                    , padding 4
                    ]

                else
                    []
               )
        )
    <|
        el
            [ width fill
            , pointer
            , case model.wordCategorySelect of
                True ->
                    inFront <|
                        column
                            [ Border.color (rgb255 220 131 0)
                            , Border.width 4
                            , Background.color (rgb255 23 35 60)
                            , width fill
                            , htmlAttribute <| Html.Attributes.style "z-index" "10"
                            ]
                        <|
                            List.map
                                (\cat ->
                                    el
                                        ([ onClickStopPropagation <| ToggleCategory cat
                                         , width fill
                                         ]
                                            ++ (if List.member cat model.selectedCategories then
                                                    [ Background.color (rgb255 255 152 0)
                                                    , Font.color (rgb255 23 35 60)
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                                        (text cat)
                                )
                                (Dict.keys model.wordList)

                False ->
                    onClick <| ToggleCategorySelection True
            ]
            (if List.isEmpty model.selectedCategories then
                text "Select"

             else
                paragraph [] <| List.intersperse (text ", ") <| List.map text model.selectedCategories
            )


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    htmlAttribute <| stopPropagationOn "click" (Decode.succeed ( msg, True ))


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
    column
        [ centerX
        , centerY
        , Font.size 50
        , Font.center
        ]
        [ el [ Font.center ] <| text <| Maybe.withDefault "?" <| List.head model.seenWords
        , el [ Font.center, Font.color (rgb255 255 255 255) ] <| text <| Maybe.withDefault "Whoops!" <| Maybe.map String.fromInt model.gameTimer
        ]
