module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Task
import Time exposing (Time, second, inSeconds)
import List.Extra


---- MODEL ----


type alias Model =
    { startTime : Maybe Time
    , currentTime : Maybe Time
    , allocatedTime : Time
    , timeLeft : Time
    , faces : List String
    , currentFaceIndex : Int
    }


init : ( Model, Cmd Msg )
init =
    { startTime = Nothing
    , currentTime = Nothing
    , allocatedTime = 5 * Time.minute
    , timeLeft = 5 * Time.minute
    , faces =
        [ "Justin.png"
        , "Gabriel.png"
        ]
    , currentFaceIndex = 0
    }
        ! [ getStartTime ]


getStartTime : Cmd Msg
getStartTime =
    Task.perform SetStartTime Time.now



---- UPDATE ----


type Msg
    = NoOp
    | SetStartTime Time
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetStartTime time ->
            { model | startTime = Just time } ! []

        Tick time ->
            if timeLeft model <= 0 then
                handleTimeout model
            else
                { model
                    | currentTime = Just time
                    , timeLeft = timeLeft model
                }
                    ! []


computeTimeLeft : Time -> Time -> Time -> Time
computeTimeLeft allocatedTime startTime currentTime =
    allocatedTime + startTime - currentTime


timeLeft : Model -> Time
timeLeft model =
    Maybe.map3 computeTimeLeft (Just model.allocatedTime) model.startTime model.currentTime
        |> Maybe.withDefault model.allocatedTime


handleTimeout : Model -> ( Model, Cmd Msg )
handleTimeout model =
    changeFace model
        ! []


changeFace : Model -> Model
changeFace model =
    { model | currentFaceIndex = (model.currentFaceIndex + 1) % List.length model.faces }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ faceImage model
        , div [] [ text ("Time left: " ++ (model.timeLeft |> Time.inSeconds |> toString)) ]
        ]


faceImage : Model -> Html Msg
faceImage model =
    let
        faceUrl =
            Maybe.withDefault "" (List.head model.faces)
    in
        img [ src faceUrl ] []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
