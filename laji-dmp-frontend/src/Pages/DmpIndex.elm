module Pages.DmpIndex exposing (..)
import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (ul)
import Html exposing (li)

type alias Model = {}

type Msg = Empty

init : ( Model, Cmd Msg )
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
  { title = "Dmp Index View"
  , body =
    [ navigation
    , ul []
      [ li []
        [ a [href "/dmp/123"] [text "DMP 123"] ]
      ]
    , a [href "/dmp/new"] [text "New DMP"]
    ]
  }
