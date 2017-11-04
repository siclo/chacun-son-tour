module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Task
import Time exposing (Time, second, inSeconds)


---- MODEL ----


type alias Model =
    { startTime : Maybe Time
    , currentTime : Maybe Time
    , allocatedTime : Time
    , timeLeft : Time
    }


init : ( Model, Cmd Msg )
init =
    { startTime = Nothing
    , currentTime = Nothing
    , allocatedTime = 5 * Time.minute
    , timeLeft = 5 * Time.minute
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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/Gabriel.png" ] []
        , div [] [ text ("Time left: " ++ (model.timeLeft |> Time.inSeconds |> toString)) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
