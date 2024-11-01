module Pages.DmpIndex exposing (..)
import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (Html)
import Html exposing (div)

type alias Model = {}

type Msg = Empty

init : ( Model, Cmd Msg )
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Index View"
  , body = div []
    [ ul []
      [ li []
        [ a [href "/dmp/123"] [text "DMP 123"] ]
      ]
    , a [href "/dmp/new"] [text "New DMP"]
    ]
  }
