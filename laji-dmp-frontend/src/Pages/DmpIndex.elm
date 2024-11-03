module Pages.DmpIndex exposing (..)

import Http
import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (Html)
import Html exposing (div)
import Config exposing (config)
import Array
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd
import DmpApi exposing (DmpList)
import DmpApi exposing (getDmpList)
import DmpApi exposing (DataManagementPlan)

type Model = Loading | Error | DmpList DmpList

type Msg = GotDmpListResponse (Result Http.Error DmpList)

init : ( Model, Cmd Msg )
init = (Loading, getDmpList GotDmpListResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDmpListResponse res ->
      case res of
        Ok dmpList -> (DmpList dmpList, Cmd.none)
        Err e ->
          let _ = Debug.log "Error loading DMP list" e
          in (Error, Cmd.none)

dmpListElementView : DataManagementPlan -> Html msg
dmpListElementView elem =
  case elem.id of
    Just id ->
      li [] [ a [href <| "dmp/" ++ String.fromInt id] [text elem.testField] ]
    Nothing -> li [] [text "Error: expected DMP to have an id"]

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Index View"
  , body = div []
    <| case model of
      Error -> [ text "Failed to load the list of DMPs." ]
      Loading -> [ text "Loading the list of DMPs..." ]
      DmpList dmpList -> 
        [
          ul []
            <| Array.toList <| Array.map dmpListElementView dmpList
          , a [href "/dmp/new"] [text "New DMP"]
        ]
  }
