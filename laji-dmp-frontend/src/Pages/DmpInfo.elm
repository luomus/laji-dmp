module Pages.DmpInfo exposing (..)

import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (Html)
import DmpApi exposing (DataManagementPlan)
import DmpApi exposing (getDmp)
import Pages.DmpIndex exposing (Msg(..))
import Http
import Platform.Cmd as Cmd
import Html exposing (div)

type Model = Error | Loading | Dmp DataManagementPlan

type Msg = GotDmpResponse (Result Http.Error DataManagementPlan)

init : String -> ( Model, Cmd Msg )
init str =
  let maybeId = String.toInt str
  in case maybeId of
    Just id ->
      (Loading, getDmp id GotDmpResponse)
    Nothing -> (Error, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDmpResponse res ->
      case res of
        Ok dmp -> (Dmp dmp, Cmd.none)
        Err e ->
          let _ = Debug.log "Error loading DMP" e
          in (Error, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Info View"
  , body =
    div [] <| case model of
      Dmp dmp ->
        case dmp.id of
          Just id ->
            [ div [] [ a [href <| "/dmp/" ++ (String.fromInt id) ++"/edit"] [text "Edit"] ]
            , div [] [ text <| "Id: " ++ (String.fromInt id) ]
            , div [] [ text <| "Test field: " ++ dmp.testField ]
            ]
          Nothing -> [text "Error: expected DMP to have an id"]
      Loading -> [text "Loading data management plan..."]
      Error -> [text "Error loading data management plan"]
  }
