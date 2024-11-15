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
import DmpApi exposing (Dataset)
import Array
import DmpApi exposing (Distribution)
import DmpApi exposing (Host)

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

hostView : Maybe Host -> Html Msg
hostView maybeHost = case maybeHost of
  Just host -> div []
    [ div [] [text <| Debug.toString host.backupFrequency]
    , div [] [text <| Debug.toString host.geoLocation]
    ]
  Nothing -> div [] []

distributionView : Distribution -> Html Msg
distributionView distribution = div []
  [ div [] [text <| Debug.toString distribution.dataAccess] -- TODO
  , div [] [text <| Debug.toString distribution.accessUrl] -- TODO
  , hostView distribution.host
  ]

datasetView : Dataset -> Html Msg
datasetView dataset = div []
  [ div [] [text dataset.title]
  , div [] [text <| Debug.toString dataset.personalData] -- TODO
  , div [] <| Array.toList <| Array.map distributionView dataset.distributions
  ]

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
            , div [] <| Array.toList <| Array.map datasetView dmp.datasets
            ]
          Nothing -> [text "Error: expected DMP to have an id"]
      Loading -> [text "Loading data management plan..."]
      Error -> [text "Error loading data management plan"]
  }
