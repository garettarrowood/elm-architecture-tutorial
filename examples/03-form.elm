import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String

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
      if passwordLength model then
        ("red", "Password must be 8 or more characters")
      else if model.password /= model.passwordAgain then
        ("red", "Passwords do not match")
      else
        ("green", "Your password passes validations")
  in
    div [ style [("color", color)] ] [ text message ]

passwordLength : Model -> Bool
passwordLength model =
  String.length model.password < 8
