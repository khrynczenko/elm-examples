module Forms exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


minimumPasswordLength : Int
minimumPasswordLength =
    8


isPasswordLengthValid : String -> Bool
isPasswordLengthValid str =
    String.length str > minimumPasswordLength


type PasswordValidationError
    = TooShort
    | NoUpperCase
    | NoLowerCase
    | NoNumeric


passwordValidationErrorToString : PasswordValidationError -> String
passwordValidationErrorToString err =
    case err of
        TooShort ->
            "Password must exceed " ++ String.fromInt minimumPasswordLength ++ "characters!"

        NoUpperCase ->
            "Password must contain at least on uppercase character!"

        NoLowerCase ->
            "Password must contain at least on lowercase character!"

        NoNumeric ->
            "Password must contain at least on digit!"


isPasswordValid : String -> Result PasswordValidationError ()
isPasswordValid str =
    if String.length str <= minimumPasswordLength then
        Err TooShort

    else if not <| List.any Char.isLower (String.toList str) then
        Err NoLowerCase

    else if not <| List.any Char.isUpper (String.toList str) then
        Err NoUpperCase

    else
        Ok ()


viewValidation : Model -> Html msg
viewValidation model =
    if model.password /= model.passwordAgain then
        div [ style "color" "red" ] [ text "Passwords do not match!" ]

    else if
        case isPasswordValid model.password of
            Ok () ->
                False

            Err _ ->
                True
    then
        case isPasswordValid model.password of
            Ok () ->
                div [] []

            Err e ->
                div [ style "color" "red" ]
                    [ text <|
                        passwordValidationErrorToString e
                    ]

    else
        div [ style "color" "green" ] [ text "OK" ]
