module Pages.DmpNew exposing (..)

import Html exposing (Html)
import Browser.Navigation as Nav
import Views.DmpEditor exposing (EditorMode(..))
import User
import Config exposing (Config)

type alias Model = Views.DmpEditor.Model

type alias Msg = Views.DmpEditor.Msg

init : Nav.Key -> User.LoginSession -> ( Model, Cmd Msg )
init key = Views.DmpEditor.init key New

update : Config -> Msg -> Model -> (Model, Cmd Msg)
update = Views.DmpEditor.update

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp New View"
  , body = Views.DmpEditor.view model
  }
