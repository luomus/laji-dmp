module Pages.DmpInfo exposing (..)

import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (Html)

type alias Model = {}

type Msg = Empty

init : ( Model, Cmd Msg )
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Info View"
  , body = a [href "/dmp/123/edit"] [text "DMP 123 Edit"]
  }
