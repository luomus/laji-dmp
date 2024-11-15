module Views.DmpEditor exposing (..)

import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (Html)
import DmpApi exposing (DataManagementPlan)
import Platform.Cmd as Cmd
import DmpApi exposing (newDmp)
import Http exposing (Error)
import Html exposing (div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Html exposing (input)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Browser.Navigation as Nav
import DmpApi exposing (editDmp)
import Array
import Html.Attributes exposing (class)
import DmpApi exposing (PersonalData(..))
import DmpApi exposing (Dataset)
import Html exposing (label)
import Html.Attributes exposing (id)
import Html.Attributes exposing (for)
import List exposing (map)
import DmpApi exposing (Distribution)
import DmpApi exposing (Host)
import DmpApi exposing (DataAccess)
import Platform.Cmd as Cmd
import DmpApi exposing (DataAccess(..))
import Maybe exposing (withDefault)

type ModelStatus = Editing | Submitting | SubmitError Error

type EditorMode = New | Edit String

type alias Model =
  { dmp: DataManagementPlan
  , status: ModelStatus
  , key: Nav.Key
  , mode: EditorMode
  }

type Msg
  = OnSubmit
  | GotDmpApiResponse (Result Error String)
  | OnAddDataset
  | OnRemoveDataset Int
  | OnModifyDatasetTitle Int String
  | OnModifyDatasetPersonalData Int PersonalData
  | OnAddDistribution Int
  | OnRemoveDistribution Int Int
  | OnModifyDistributionDataAccess Int Int DataAccess
  | OnModifyDistributionAccessUrl Int Int (Maybe String)
  | OnToggleHost Int Int
  | OnModifyHostBackupFrequency Int Int (Maybe String)
  | OnModifyHostGeoLocation Int Int (Maybe String)

init : Nav.Key -> EditorMode -> ( Model, Cmd Msg )
init key mode =
  ( { dmp = { id = Nothing, datasets = Array.fromList [] }
    , status = Editing
    , key = key
    , mode = mode
    }
  , Cmd.none
  )

removeAt : Int -> Array.Array a -> Array.Array a
removeAt idx array =
  array
    |> Array.toList
    |> List.indexedMap (\i value -> (i, value))
    |> List.filter (\(i, _) -> i /= idx)
    |> List.map Tuple.second
    |> Array.fromList

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnAddDataset ->
      let
        updateDmp dmp =
          { dmp | datasets =
            Array.push { title = "", personalData = Unknown, distributions = Array.empty } dmp.datasets
          }
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnRemoveDataset datasetIdx ->
      let
        updateDmp dmp = { dmp | datasets = removeAt datasetIdx dmp.datasets}
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnModifyDatasetTitle idx title ->
      let
        updateDmp dmp =
          case Array.get idx dmp.datasets of
            Just dataset -> { dmp | datasets = Array.set idx ({ dataset | title = title }) dmp.datasets }
            Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnModifyDatasetPersonalData idx personalData ->
      let
        updateDmp dmp =
          case Array.get idx dmp.datasets of
            Just dataset -> { dmp | datasets = Array.set idx ({ dataset | personalData = personalData }) dmp.datasets }
            Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnAddDistribution idx ->
      let
        updateDmp dmp = case Array.get idx dmp.datasets of
          Just dataset ->
            { dmp | datasets =
              Array.set idx { dataset | distributions =
                Array.push { dataAccess = Open, accessUrl = Nothing, host = Nothing } dataset.distributions
              } dmp.datasets }
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnRemoveDistribution datasetIdx distributionIdx ->
      let
        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
          Just dataset ->
            { dmp | datasets = Array.set datasetIdx { dataset | distributions = removeAt distributionIdx dataset.distributions } dmp.datasets}
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnModifyDistributionDataAccess datasetIdx distributionIdx dataAccess ->
      let
        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
          Just dataset -> case Array.get distributionIdx dataset.distributions of
            Just distribution -> { dmp | datasets =
              Array.set datasetIdx { dataset | distributions =
                Array.set distributionIdx { distribution | dataAccess = dataAccess } dataset.distributions
              } dmp.datasets }
            Nothing -> dmp
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnModifyDistributionAccessUrl datasetIdx distributionIdx accessUrl ->
      let
        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
          Just dataset -> case Array.get distributionIdx dataset.distributions of
            Just distribution -> { dmp | datasets =
              Array.set datasetIdx { dataset | distributions =
                Array.set distributionIdx { distribution | accessUrl = accessUrl } dataset.distributions
              } dmp.datasets }
            Nothing -> dmp
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnToggleHost datasetIdx distributionIdx ->
      let
        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
          Just dataset -> case Array.get distributionIdx dataset.distributions of
            Just distribution -> { dmp | datasets =
              Array.set datasetIdx { dataset | distributions =
                Array.set distributionIdx { distribution | host =
                  case distribution.host of
                    Just _ -> Nothing
                    Nothing -> Just { backupFrequency = Nothing, geoLocation = Nothing }
                  } dataset.distributions
              } dmp.datasets }
            Nothing -> dmp
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnModifyHostBackupFrequency datasetIdx distributionIdx backupFrequency ->
      let
        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
          Just dataset -> case Array.get distributionIdx dataset.distributions of
            Just distribution -> case distribution.host of
              Just host -> { dmp | datasets =
                Array.set datasetIdx { dataset | distributions =
                  Array.set distributionIdx { distribution | host =
                    Just { host | backupFrequency = backupFrequency }} dataset.distributions
                } dmp.datasets }
              Nothing -> dmp
            Nothing -> dmp
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnModifyHostGeoLocation datasetIdx distributionIdx geoLocation ->
      let
        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
          Just dataset -> case Array.get distributionIdx dataset.distributions of
            Just distribution -> case distribution.host of
              Just host -> { dmp | datasets =
                Array.set datasetIdx { dataset | distributions =
                  Array.set distributionIdx { distribution | host =
                    Just { host | geoLocation = geoLocation }} dataset.distributions
                } dmp.datasets }
              Nothing -> dmp
            Nothing -> dmp
          Nothing -> dmp
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnSubmit -> case model.status of
      Submitting -> (model, Cmd.none)
      _-> case model.mode of
        New -> ({ model | status = Submitting }, newDmp model.dmp GotDmpApiResponse)
        Edit id -> ({ model | status = Submitting }, editDmp id model.dmp GotDmpApiResponse)
    GotDmpApiResponse res -> case res of
      Ok str -> 
        (model, Nav.pushUrl model.key "/dmp")
      Err e ->
        ({ model | status = SubmitError e }, Cmd.none)

hostEditorView : Maybe Host -> Int -> Int -> Bool -> Html Msg
hostEditorView maybeHost datasetIdx distributionIdx d = div []
  [ div [] <| case maybeHost of
    Just host ->
      [ div []
        [ label [ for "host-backup-frequency" ] [ text "Backup frequency" ]
        , input
          [ id "host-backup-frequency"
          , value <| withDefault "" host.backupFrequency
          , disabled d
          , onInput <| (\str -> OnModifyHostBackupFrequency datasetIdx distributionIdx (
            case str of
              "" -> Nothing
              s -> Just s
          ))
          ]
          []
        ]
      , div []
        [ label [ for "host-geo-location" ] [ text "Geo location" ]
        , input
          [ id "host-geo-location"
          , value <| withDefault "" host.geoLocation
          , disabled d
          , onInput <| (\str -> OnModifyHostGeoLocation datasetIdx distributionIdx (
            case str of
              "" -> Nothing
              s -> Just s
          ))
          ]
          []
        ]
      ]
    Nothing -> []
  , button
    [ onClick <| OnToggleHost datasetIdx distributionIdx
    , disabled d
    , class "btn"
    ]
    [ text "Toggle Host" ]
  ]

distributionEditorView : Distribution -> Int -> Int -> Bool -> Html Msg
distributionEditorView distribution datasetIdx distributionIdx d = div []
  [ div []
    [ label [ for "distribution-data-access" ] [ text "Data access" ]
    , input -- TODO should be <select>
      [ id "distribution-data-access"
      , value <| Debug.toString distribution.dataAccess
      , disabled d
      , onInput <| OnModifyDatasetTitle datasetIdx
      ]
      []
    ]
  , div []
    [ label [ for "distribution-access-url" ] [ text "Access url" ]
    , input
      [ id "distribution-access-url"
      , value <| withDefault "" distribution.accessUrl
      , disabled d
      , onInput <| (\str -> OnModifyDistributionAccessUrl datasetIdx distributionIdx (
        case str of
          "" -> Nothing
          s -> Just s
      ))
      ]
      []
    ]
  , hostEditorView distribution.host datasetIdx distributionIdx d
  , button
    [ onClick <| OnRemoveDistribution datasetIdx distributionIdx
    , disabled d
    , class "btn"
    ]
    [ text "- Remove distribution" ]
  ]

datasetEditorView : Dataset -> Int -> Bool -> Html Msg
datasetEditorView dataset datasetIdx d = div []
  [ div []
    [ label [ for "dataset-editor-title" ] [ text "Title" ]
    , input
      [ id "dataset-editor-title"
      , value dataset.title
      , disabled d
      , onInput <| OnModifyDatasetTitle datasetIdx
      ]
      []
    ]
  , div []
    [ label [ for "dataset-editor-personal-data" ] [ text "Personal Data" ]
    , input -- TODO should be <select>
      [ id "dataset-editor-personal-data"
      , value dataset.title
      , disabled d
      , onInput <| (\str -> OnModifyDatasetPersonalData datasetIdx Yes)
      ]
      []
    ]
  , div []
    [ div [] <| Array.toList <| Array.indexedMap (\distributionIdx dist -> distributionEditorView dist datasetIdx distributionIdx d) dataset.distributions
    , button
      [ onClick <| OnAddDistribution datasetIdx
      , disabled d
      , class "btn"
      ]
      [ text "+ Add distribution" ]
    ]
  , button
    [ onClick <| OnRemoveDataset datasetIdx
    , disabled d
    , class "btn"
    ]
    [ text "- Remove dataset" ]
  ]

dmpEditorView : DataManagementPlan -> Bool -> Html Msg
dmpEditorView dmp d = div []
  [ div [] <| Array.toList <| Array.indexedMap (\idx ds -> datasetEditorView ds idx d) dmp.datasets
  , button
    [ onClick OnAddDataset
    , disabled d
    , class "btn"
    ]
    [ text "+ Add dataset" ]
  ]

editorFormView : Model -> Html Msg
editorFormView model = 
  div []
  [ div []
      [ dmpEditorView model.dmp (model.status == Submitting)
      , button
        [ onClick OnSubmit
        , disabled (model.status == Submitting)
        , class "btn btn-primary"
        ]
        [ text "Submit" ]
      ]
  , div [] <| case model.status of
    SubmitError e -> [ text <| "Error submitting DMP: " ++ Debug.toString e ]
    _ -> []
  ]

view : Model -> Html Msg
view model = editorFormView model
