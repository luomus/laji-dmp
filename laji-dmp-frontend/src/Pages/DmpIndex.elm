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
import Html exposing (h5)
import User exposing (LoginSession)

type DmpListState = Loading | Error | DmpList DmpList

type alias Model =
  { dmpList: DmpListState
  , session: LoginSession
  }

type Msg = GotDmpListResponse (Result Http.Error DmpList)

init : LoginSession -> ( Model, Cmd Msg )
init session = ({ dmpList = Loading, session = session }, getDmpList GotDmpListResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDmpListResponse res ->
      case res of
        Ok dmpList -> ({ model | dmpList = DmpList dmpList }, Cmd.none)
        Err e ->
          let _ = Debug.log "Error loading DMP list" e
          in ({ model | dmpList = Error }, Cmd.none)

dmpElementView : DataManagementPlan -> Html msg
dmpElementView elem =
  case elem.id of
    Just id -> a [ href <| "dmp/" ++ String.fromInt id, class "dmp-index-dmp-box" ]
      [ h5 [] [ text <| "DMP " ++ String.fromInt id ]
      , div [] [ text <| "Organization: " ++ elem.orgId ]
      , div [] [ text <| String.fromInt (Array.length elem.datasets) ++ " datasets" ]
      ]
    Nothing -> li [] [text "Error: expected DMP to have an id"]

dmpTableView : DmpList -> Html Msg
dmpTableView dmpList = div [] (Array.toList <| Array.map dmpElementView dmpList)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Index View"
  , body = div [class "dmp-index"]
    [ div [] <| case model.dmpList of
      Error -> [ text "Failed to load the list of DMPs." ]
      Loading -> [ text "Loading the list of DMPs..." ]
      DmpList dmpList -> 
        [
          dmpTableView dmpList
        ]
    , div [] <| case model.session of
      User.LoggedIn personToken personResponse ->
        [ a [href "/dmp/new", class "btn btn-primary"] [text "+ New DMP"] ]
      _ -> []
    ]
  }
