module ClockTime exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SwitchClockState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if .paused model then
                ( model
                , Cmd.none
                )

            else
                ( { model | time = newTime }
                , Cmd.none
                )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        SwitchClockState ->
            ( { model | paused = not (.paused model) }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)
    in
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , button [ onClick SwitchClockState ] [ text "Pause/Start" ]
        ]
