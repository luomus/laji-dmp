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
import Array
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd
import Html.Attributes exposing (class)
import Html exposing (table)
import Html exposing (tr)
import Html exposing (th)
import Html exposing (td)
import Html exposing (h5)
import User exposing (LoginSession)
import DmpApi exposing (getDmpList)
import Models exposing (Dmp)
import Config exposing (Config)
import Dict exposing (Dict)
import Organization exposing (Organization)
import Utils exposing (showOrgName)
import Organization exposing (OrgLookup)

type DmpListState = Loading | Error String | DmpList (Array.Array Dmp)

type alias Model =
  { dmpList: DmpListState
  , session: LoginSession
  }

type Msg = GotDmpListResponse (Result Http.Error (Array.Array Dmp))

init : Config -> LoginSession -> ( Model, Cmd Msg )
init cfg session = ({ dmpList = Loading, session = session }, getDmpList cfg GotDmpListResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDmpListResponse res ->
      case res of
        Ok dmpList -> ({ model | dmpList = DmpList dmpList }, Cmd.none)
        Err e ->
          ({ model | dmpList = Error "Failed to load DMP list response" }, Cmd.none )

dmpElementView : Dmp -> OrgLookup -> Html msg
dmpElementView elem orgs =
  case elem.dmpId of
    Just id -> a [ href <| "dmp/" ++ String.fromInt id, class "dmp-index-dmp-box" ]
      [ h5 [] [ text <| elem.dmpTitle ]
      , div [] [ text <| "Organisaatio: " ++ showOrgName elem orgs ]
      , div [] [ text <| String.fromInt (Array.length elem.dmpDatasets) ++ " aineistoa" ]
      ]
    Nothing -> li [] [text "Virhe: DMP:n tunniste puuttuu"]

dmpTableView : Array.Array Dmp -> OrgLookup -> Html Msg
dmpTableView dmpList orgs = div [] (Array.toList <| Array.map (\a -> dmpElementView a orgs) dmpList)

view : Model -> OrgLookup -> { title : String, body : Html Msg }
view model orgs =
  { title = "DMP luettelo"
  , body = div [class "dmp-index"]
    [ div [] <| case model.dmpList of
      Error err -> [ text <| "Virhe: " ++ err ]
      Loading -> [ text "Ladataan DMP luetteloa..." ]
      DmpList dmpList -> 
        [
          dmpTableView dmpList orgs
        ]
    , div [] <| case model.session of
      User.LoggedIn personToken personResponse ->
        if Array.isEmpty personResponse.organisation
          then [ text "Huomio: luodaksesi uuden DMP:n käyttäjätunnuksesi tulee kuulua johonkin organisaatioon." ]
          else [ a [href "/dmp/new", class "btn btn-primary"] [text "+ Uusi DMP"] ]
      _ -> []
    ]
  }
