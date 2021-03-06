module RandomExample exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dieFace = 1 }
    , Cmd.none
    )


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text
                (case model.dieFace of
                    1 ->
                        String.fromChar (Char.fromCode 0x2680)

                    2 ->
                        String.fromChar (Char.fromCode 0x2681)

                    3 ->
                        String.fromChar (Char.fromCode 0x2682)

                    4 ->
                        String.fromChar (Char.fromCode 0x2683)

                    5 ->
                        String.fromChar (Char.fromCode 0x2684)

                    6 ->
                        String.fromChar (Char.fromCode 0x2685)

                    _ ->
                        "Impossible"
                )
            ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
