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
import Html.Attributes exposing (class)
import Html exposing (table)
import Html exposing (tr)
import Html exposing (th)
import Html exposing (td)

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

dmpTableRowView : DataManagementPlan -> Html msg
dmpTableRowView elem =
  case elem.id of
    Just id -> tr []
      [ td [] [ a [href <| "dmp/" ++ String.fromInt id] [text <| String.fromInt id] ]
      , td [] [ text elem.testField ]
      ]
    Nothing -> li [] [text "Error: expected DMP to have an id"]

dmpTableView : DmpList -> Html Msg
dmpTableView dmpList = table [] <|
  [ tr [] [th [] [text "Id"], th [] [text "Test field"]]
  ] ++ (Array.toList <| Array.map dmpTableRowView dmpList)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Index View"
  , body = div [class "dmp-index"]
    <| case model of
      Error -> [ text "Failed to load the list of DMPs." ]
      Loading -> [ text "Loading the list of DMPs..." ]
      DmpList dmpList -> 
        [
          dmpTableView dmpList
          , a [href "/dmp/new", class "btn btn-primary"] [text "+ New DMP"]
        ]
  }
