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
import Html.Attributes exposing (class)
import Html exposing (h2)
import Html exposing (h3)
import Html exposing (h4)
import Html exposing (h5)

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
  Just host -> div [ class "dmp-editor-host" ]
    [ h5 [] [ text "Host" ]
    , div [] [text <| "Backup frequency: " ++ Debug.toString host.backupFrequency]
    , div [] [text <| "Geo location: " ++ Debug.toString host.geoLocation]
    ]
  Nothing -> div [] []

distributionView : Int -> Distribution -> Html Msg
distributionView distributionIdx distribution = div [ class "dmp-editor-distribution" ]
  [ h4 [] [ text <| "Distribution " ++ (String.fromInt distributionIdx) ]
  , div [] [text <| "Data access: " ++ Debug.toString distribution.dataAccess] -- TODO
  , div [] [text <| "Access URL: " ++ Debug.toString distribution.accessUrl] -- TODO
  , hostView distribution.host
  ]

datasetView : Int -> Dataset -> Html Msg
datasetView datasetIdx dataset = div [ class "dmp-editor-dataset" ]
  [ h3 [] [ text <| "Dataset " ++ (String.fromInt datasetIdx) ]
  , div [] [text <| "Title: " ++ dataset.title]
  , div [] [text <| "Personal data: " ++ Debug.toString dataset.personalData]
  , div [] <| Array.toList <| Array.indexedMap distributionView dataset.distributions
  ]

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Info View"
  , body =
    div [] <| case model of
      Dmp dmp ->
        case dmp.id of
          Just id ->
            [ h2 [] [ text <| "DMP " ++ (String.fromInt id) ++ " info" ]
            , div [] [ a [href <| "/dmp/" ++ (String.fromInt id) ++"/edit", class "btn"] [text "Edit"] ]
            , div [] <| Array.toList <| Array.indexedMap datasetView dmp.datasets
            ]
          Nothing -> [text "Error: expected DMP to have an id"]
      Loading -> [text "Loading data management plan..."]
      Error -> [text "Error loading data management plan"]
  }
