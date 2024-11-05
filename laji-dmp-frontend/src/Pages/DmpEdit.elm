module Pages.DmpEdit exposing (..)

import Html exposing (Html)
import Browser.Navigation as Nav
import Views.DmpEditor exposing (EditorMode(..))

type alias Model = Views.DmpEditor.Model

type alias Msg = Views.DmpEditor.Msg

init : Nav.Key -> String -> ( Model, Cmd Msg )
init key id = Views.DmpEditor.init key <| Edit id

update : Msg -> Model -> (Model, Cmd Msg)
update = Views.DmpEditor.update

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Edit View"
  , body = Views.DmpEditor.view model
  }
