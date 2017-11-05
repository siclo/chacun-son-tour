module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import List.Extra
import Random
import Svg
import Svg.Attributes
import Task
import Time exposing (Time, millisecond, second, inSeconds)


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
    let
        initialModel =
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
    in
        initialModel ! [ getStartTime, choseStartFace initialModel ]


getStartTime : Cmd Msg
getStartTime =
    Task.perform SetStartTime Time.now


choseStartFace : Model -> Cmd Msg
choseStartFace model =
    Random.generate StartFaceIndex (Random.int 0 (List.length model.faces))



---- UPDATE ----


type Msg
    = NoOp
    | SetStartTime Time
    | StartFaceIndex Int
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetStartTime time ->
            { model | startTime = Just time } ! []

        StartFaceIndex faceIndex ->
            { model | currentFaceIndex = faceIndex } ! []

        Tick time ->
            let
                newTimeLeft =
                    timeLeft model.allocatedTime model.startTime (Just time)
            in
                if newTimeLeft <= 0 then
                    handleTimeout model
                else
                    { model
                        | currentTime = Just time
                        , timeLeft = newTimeLeft
                    }
                        ! []


timeLeft : Time -> Maybe Time -> Maybe Time -> Time
timeLeft allocatedTime startTime currentTime =
    Maybe.map3 computeTimeLeft (Just allocatedTime) startTime currentTime
        |> Maybe.withDefault allocatedTime


computeTimeLeft : Time -> Time -> Time -> Time
computeTimeLeft allocatedTime startTime currentTime =
    allocatedTime + startTime - currentTime


handleTimeout : Model -> ( Model, Cmd Msg )
handleTimeout model =
    model
        |> changeFace
        |> resetTimeLeft
        |> flip (!) []


changeFace : Model -> Model
changeFace model =
    { model | currentFaceIndex = (model.currentFaceIndex + 1) % List.length model.faces }


resetTimeLeft : Model -> Model
resetTimeLeft model =
    { model
        | timeLeft = model.allocatedTime
        , startTime = Maybe.map2 (+) model.currentTime (Just model.timeLeft)
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ faceImage model
        , div [] [ hourglass model ]
        ]


faceImage : Model -> Html Msg
faceImage model =
    let
        faceUrl =
            Maybe.withDefault "" (List.Extra.getAt model.currentFaceIndex model.faces)
    in
        img [ src faceUrl ] []


hourglass model =
    let
        timeLeftRatio =
            model.timeLeft / model.allocatedTime
    in
        Svg.svg
            [ Svg.Attributes.viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
            [ Svg.rect
                [ Svg.Attributes.x "40"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "20"
                , Svg.Attributes.height "100"
                , Svg.Attributes.fill "grey"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "40"
                , Svg.Attributes.y (toString ((1 - timeLeftRatio) * 100))
                , Svg.Attributes.width "20"
                , Svg.Attributes.height (toString (timeLeftRatio * 100))
                , Svg.Attributes.fill "blue"
                ]
                []
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
