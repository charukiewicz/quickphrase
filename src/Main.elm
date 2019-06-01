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
    , gameRoundLength : Maybe Int
    , gameRoundActive : Bool
    , gameRoundTimer : Maybe Int
    , wordList : Dict String (List String)
    , categorySelectionOpen : Bool
    , selectedCategories : List String
    , selectedWords : List String
    , seenWords : List String
    , randGen : Maybe (Random.Generator Int)
    }


type GameMode
    = TimedRound
    | TimedWord


init : () -> ( Model, Cmd Msg )
init _ =
    { gameMode = Nothing
    , gameRoundLength = Nothing
    , gameRoundActive = False
    , gameRoundTimer = Nothing
    , wordList = WordList.wordList
    , categorySelectionOpen = False
    , selectedCategories = []
    , selectedWords = []
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
    case model.gameRoundTimer of
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
    | StartRound
    | RandGen (Int -> Msg)
    | NextRound Int
    | Tick Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGameMode gm ->
            { model
                | gameMode = Just gm
                , gameRoundLength =
                    case gm of
                        TimedRound ->
                            Just 60

                        TimedWord ->
                            Just 10
            }
                |> with Cmd.none

        SetGameLength gl ->
            { model
                | gameRoundLength =
                    String.toInt gl
                        |> (\newGl ->
                                case ( newGl, gl ) of
                                    ( Nothing, "" ) ->
                                        Nothing

                                    ( Nothing, _ ) ->
                                        model.gameRoundLength

                                    ( Just _, _ ) ->
                                        newGl
                           )
            }
                |> with Cmd.none

        ToggleCategorySelection isOpen ->
            { model | categorySelectionOpen = isOpen }
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

                randGen =
                    Random.int 1 <| List.length words
            in
            { model
                | randGen = Just <| randGen
                , selectedWords = words
            }
                |> with (Random.generate NextRound randGen)

        StartRound ->
            { model
                | gameRoundActive = True
                , gameRoundTimer = model.gameRoundLength
            }
                |> with Cmd.none

        RandGen recvMsg ->
            let
                randCmd =
                    case model.randGen of
                        Just generator ->
                            Random.generate recvMsg generator

                        Nothing ->
                            Cmd.none
            in
            model
                |> with randCmd

        NextRound int ->
            let
                ( newWord, alreadySeen ) =
                    model.selectedWords
                        |> List.drop (int - 1)
                        |> List.head
                        |> Maybe.map (\word -> ( word, List.member word model.seenWords ))
                        |> Maybe.withDefault ( "", True )

                selectedWordCount =
                    List.length model.selectedWords

                seenWordCount =
                    List.length model.seenWords
            in
            if not alreadySeen then
                { model
                    | gameRoundTimer = Nothing -- Clear the timer before the next round
                    , seenWords = newWord :: model.seenWords
                }
                    |> with (Task.perform identity <| Task.succeed StartRound)

            else if seenWordCount < selectedWordCount then
                model
                    |> with (Task.perform identity <| Task.succeed <| RandGen NextRound)

            else
                { model
                    | gameRoundTimer = Nothing
                    , gameRoundActive = False
                }
                    |> with Cmd.none

        Tick time ->
            let
                newGameTimer =
                    model.gameRoundTimer
                        |> Maybe.map (\x -> x - 1)
            in
            (case newGameTimer of
                Just 0 ->
                    { model
                        | gameRoundTimer = Nothing
                        , gameRoundActive = False
                    }

                Just _ ->
                    { model | gameRoundTimer = newGameTimer }

                Nothing ->
                    { model
                        | gameRoundTimer = Nothing
                        , gameRoundActive = False
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
    { title = "QuickPhrase"
    , body =
        [ Element.layout
            ([ Background.color backgroundColor
             , Font.color primaryColor
             , Font.family [ Font.monospace ]
             ]
                ++ (if model.categorySelectionOpen then
                        [ onClick <| ToggleCategorySelection False ]

                    else if model.gameRoundActive then
                        [ onClick <| RandGen NextRound ]

                    else
                        []
                   )
            )
            (gameRender model)
        ]
    }


gameRender : Model -> Element Msg
gameRender model =
    case model.gameRoundActive of
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
        , height shrink
        ]
        [ wrappedRow
            [ spacing 30
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
            []
            [ Input.text
                [ Background.color backgroundColor
                , Border.color primaryBorderColor
                , Border.width 4
                ]
                { onChange = SetGameLength
                , text =
                    model.gameRoundLength
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault ""
                , placeholder = Just <| Input.placeholder [] (text "Seconds")
                , label = Input.labelLeft [ centerY ] (text "Round Length")
                }
            ]
        , row [ spacing 10, centerY, width fill ]
            [ text "Categories"
            , categoryControl model
            ]
        , row [ width fill ] [ startGameBtn model ]
        ]


startGameBtn : Model -> Element Msg
startGameBtn model =
    let
        readyToStart =
            model.gameMode /= Nothing && model.gameRoundLength /= Nothing && (not <| List.isEmpty model.selectedCategories)
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
    column
        [ width fill ]
    <|
        [ el
            ([ width fill
             , pointer
             , Border.color primaryBorderColor
             , Border.width 4
             , padding 10
             ]
                ++ (case model.categorySelectionOpen of
                        True ->
                            []

                        False ->
                            [ onClick <| ToggleCategorySelection True ]
                   )
            )
            (if List.isEmpty model.selectedCategories then
                text "Select"

             else
                paragraph [] <| List.intersperse (text ", ") <| List.map text model.selectedCategories
            )
        ]
            ++ (case model.categorySelectionOpen of
                    True ->
                        [ row
                            [ width fill
                            , height <| px 0
                            , inFront <|
                                (column
                                    [ Border.color primaryBorderColor
                                    , Border.width 4
                                    , Background.color backgroundColor
                                    , width fill
                                    , htmlAttribute <| Html.Attributes.style "z-index" "10"
                                    ]
                                 <|
                                    List.map
                                        (\cat ->
                                            el
                                                ([ onClickStopPropagation <| ToggleCategory cat
                                                 , width fill
                                                 , paddingXY 10 4
                                                 , pointer
                                                 ]
                                                    ++ (if List.member cat model.selectedCategories then
                                                            [ Background.color primaryColor
                                                            , Font.color backgroundColor
                                                            ]

                                                        else
                                                            []
                                                       )
                                                )
                                                (text cat)
                                        )
                                        (Dict.keys model.wordList)
                                )
                            ]
                            []
                        ]

                    False ->
                        []
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
    , Border.color primaryBorderColor
    , Border.width 4
    , padding 20
    , centerX
    , width <| minimum 350 fill
    , Font.center
    ]
        ++ (if active then
                [ Background.color primaryColor
                , Font.color backgroundColor
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
        , spacing 10
        ]
        [ el [ centerX ] <| text <| Maybe.withDefault "?" <| List.head model.seenWords
        , el [ centerX, Font.color whiteColor ] <| text <| Maybe.withDefault "Whoops!" <| Maybe.map String.fromInt model.gameRoundTimer
        ]



-- COLORS


primaryColor : Color
primaryColor =
    rgb255 255 152 0


primaryBorderColor : Color
primaryBorderColor =
    rgb255 220 131 0


backgroundColor : Color
backgroundColor =
    rgb255 23 35 60


whiteColor : Color
whiteColor =
    rgb255 255 255 255
