import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)
import Char exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

model : Model
model =
  Model "" "" ""


-- UPDATE

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


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if passwordBlank model then
        ("blue", "Please enter password")
      else if passwordMinLength model then
        ("red", "Password must be 8 or more characters")
      else if model.password /= model.passwordAgain then
        ("red", "Passwords do not match")
      else if passwordHasNumber model then
        ("red", "Password must contain at least one digit")
      else if passwordCaseCheck model then
        ("red", "Password must contain upper and lower case characters")
      else
        ("green", "Your password passes validations")
  in
    div [ style [("color", color)] ] [ text message ]

passwordMinLength : Model -> Bool
passwordMinLength model =
  length model.password < 8

passwordBlank : Model -> Bool
passwordBlank model =
  model.password == ""

passwordHasNumber : Model -> Bool
passwordHasNumber model =
  any isDigit model.password == False

passwordCaseCheck : Model -> Bool
passwordCaseCheck model =
  (any isUpper model.password && any isLower model.password) == False
