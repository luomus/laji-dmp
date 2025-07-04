module Views.DmpEditor exposing (..)

import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (Html)
import DmpApi exposing (Dmp, newDmp, editDmp)
import Platform.Cmd as Cmd
import Http exposing (Error)
import Html exposing (div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Html exposing (input)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Browser.Navigation as Nav
import Array
import Html.Attributes exposing (class)
import Html exposing (label)
import Html.Attributes exposing (id)
import Html.Attributes exposing (for)
import List exposing (map)
import Platform.Cmd as Cmd
import Maybe exposing (withDefault)
import Html exposing (h3)
import Html exposing (h4)
import Html exposing (h6)
import Html exposing (h5)
import Html exposing (h2)
import Html exposing (select)
import Html exposing (option)
import Html exposing (hr)
import Html.Attributes exposing (selected)
import User
import DmpApi exposing (UTCTime)
import DmpApi exposing (UTCTime)
import DmpApi exposing (UTCTime(..))
import DmpApi exposing (LanguageType(..))
import DmpApi exposing (DmpType(..))
import DmpApi exposing (PersonIdType(..))
import DmpApi exposing (DocumentIdType(..))

type ModelStatus = Editing | Submitting | SubmitError Error | NotLoggedInError

type EditorMode = New | Edit String

type alias Model =
  { dmp: Dmp
  , status: ModelStatus
  , key: Nav.Key
  , mode: EditorMode
  , session: User.LoginSession
  }

type Msg
  = OnSubmit
  | GotDmpApiResponse (Result Error String)
  | OnModifyDmpOrg String
--  | OnAddDataset
--  | OnRemoveDataset Int
--  | OnModifyDatasetTitle Int String
--  | OnModifyDatasetPersonalData Int PersonalData
--  | OnAddDistribution Int
--  | OnRemoveDistribution Int Int
--  | OnModifyDistributionDataAccess Int Int DataAccess
--  | OnModifyDistributionAccessUrl Int Int (Maybe String)
--  | OnToggleHost Int Int
--  | OnModifyHostBackupFrequency Int Int (Maybe String)
--  | OnModifyHostGeoLocation Int Int (Maybe String)

defaultDmp : String -> Dmp
defaultDmp org =
  { dmpId = Nothing
  , dmpCreated = Nothing
  , dmpDescription = Nothing
  , dmpLanguage = LanguageTypeFi
  , dmpModified = Nothing
  , dmpNextReviewDmp = Nothing
  , dmpOrgId = org
  , dmpTitle = ""
  , dmpTypeDmp = DmpTypeOrganizational
  , dmpContact =
    { contactMbox = ""
    , contactName = ""
    , contactContactId = { contactIdType = PersonIdTypeNone, contactIdIdentifier = Nothing }
    , contactOrganization = Nothing
    }
  , dmpDmpId = { dmpIdType = DocumentIdTypeNone, dmpIdIdentifier = Nothing }
  , dmpContributors = Array.empty
  , dmpDataLifeCycles = Array.empty
  , dmpDatasets = Array.empty
  , dmpEthicalIssues = Array.empty
  , dmpProjects = Array.empty
  }

init : Nav.Key -> EditorMode -> User.LoginSession -> ( Model, Cmd Msg )
init key mode session =
  let
    org = case session of
      User.LoggedIn _ person -> case Array.get 0 person.organisation of
        Just o -> o
        Nothing -> ""
      _ -> ""
  in
    ( { dmp = defaultDmp org
      , status = Editing
      , key = key
      , mode = mode
      , session = session
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
    OnModifyDmpOrg org ->
      let
        updateDmp dmp = { dmp | dmpOrgId = org}
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnAddDataset ->
--      let
--        updateDmp dmp =
--          { dmp | datasets =
--            Array.push { title = "", personalData = Unknown, distributions = Array.empty } dmp.datasets
--          }
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnRemoveDataset datasetIdx ->
--      let
--        updateDmp dmp = { dmp | datasets = removeAt datasetIdx dmp.datasets}
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnModifyDatasetTitle idx title ->
--      let
--        updateDmp dmp =
--          case Array.get idx dmp.datasets of
--            Just dataset -> { dmp | datasets = Array.set idx ({ dataset | title = title }) dmp.datasets }
--            Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnModifyDatasetPersonalData idx personalData ->
--      let
--        updateDmp dmp =
--          case Array.get idx dmp.datasets of
--            Just dataset -> { dmp | datasets = Array.set idx ({ dataset | personalData = personalData }) dmp.datasets }
--            Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnAddDistribution idx ->
--      let
--        updateDmp dmp = case Array.get idx dmp.datasets of
--          Just dataset ->
--            { dmp | datasets =
--              Array.set idx { dataset | distributions =
--                Array.push { dataAccess = Open, accessUrl = Nothing, host = Nothing } dataset.distributions
--              } dmp.datasets }
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnRemoveDistribution datasetIdx distributionIdx ->
--      let
--        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
--          Just dataset ->
--            { dmp | datasets = Array.set datasetIdx { dataset | distributions = removeAt distributionIdx dataset.distributions } dmp.datasets}
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnModifyDistributionDataAccess datasetIdx distributionIdx dataAccess ->
--      let
--        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
--          Just dataset -> case Array.get distributionIdx dataset.distributions of
--            Just distribution -> { dmp | datasets =
--              Array.set datasetIdx { dataset | distributions =
--                Array.set distributionIdx { distribution | dataAccess = dataAccess } dataset.distributions
--              } dmp.datasets }
--            Nothing -> dmp
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnModifyDistributionAccessUrl datasetIdx distributionIdx accessUrl ->
--      let
--        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
--          Just dataset -> case Array.get distributionIdx dataset.distributions of
--            Just distribution -> { dmp | datasets =
--              Array.set datasetIdx { dataset | distributions =
--                Array.set distributionIdx { distribution | accessUrl = accessUrl } dataset.distributions
--              } dmp.datasets }
--            Nothing -> dmp
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnToggleHost datasetIdx distributionIdx ->
--      let
--        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
--          Just dataset -> case Array.get distributionIdx dataset.distributions of
--            Just distribution -> { dmp | datasets =
--              Array.set datasetIdx { dataset | distributions =
--                Array.set distributionIdx { distribution | host =
--                  case distribution.host of
--                    Just _ -> Nothing
--                    Nothing -> Just { backupFrequency = Nothing, geoLocation = Nothing }
--                  } dataset.distributions
--              } dmp.datasets }
--            Nothing -> dmp
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnModifyHostBackupFrequency datasetIdx distributionIdx backupFrequency ->
--      let
--        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
--          Just dataset -> case Array.get distributionIdx dataset.distributions of
--            Just distribution -> case distribution.host of
--              Just host -> { dmp | datasets =
--                Array.set datasetIdx { dataset | distributions =
--                  Array.set distributionIdx { distribution | host =
--                    Just { host | backupFrequency = backupFrequency }} dataset.distributions
--                } dmp.datasets }
--              Nothing -> dmp
--            Nothing -> dmp
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
--    OnModifyHostGeoLocation datasetIdx distributionIdx geoLocation ->
--      let
--        updateDmp dmp = case Array.get datasetIdx dmp.datasets of
--          Just dataset -> case Array.get distributionIdx dataset.distributions of
--            Just distribution -> case distribution.host of
--              Just host -> { dmp | datasets =
--                Array.set datasetIdx { dataset | distributions =
--                  Array.set distributionIdx { distribution | host =
--                    Just { host | geoLocation = geoLocation }} dataset.distributions
--                } dmp.datasets }
--              Nothing -> dmp
--            Nothing -> dmp
--          Nothing -> dmp
--      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnSubmit -> case model.status of
      Submitting -> (model, Cmd.none)
      _-> case model.session of
        User.LoggedIn personToken person ->
          case model.mode of
            New -> ({ model | status = Submitting }, newDmp model.dmp personToken GotDmpApiResponse)
            Edit id -> ({ model | status = Submitting }, editDmp id model.dmp personToken GotDmpApiResponse)
        _ -> ({ model | status = NotLoggedInError }, Cmd.none)
    GotDmpApiResponse res -> case res of
      Ok str -> 
        (model, Nav.pushUrl model.key "/dmp")
      Err e ->
        ({ model | status = SubmitError e }, Cmd.none)

-- hostEditorView : Host -> Int -> Int -> Bool -> Html Msg
-- hostEditorView host datasetIdx distributionIdx d = div [ class "dmp-editor-host" ]
--   [ div []
--     [ h5 [] [ text <| "Host"]
--     , div [ class "form-field" ]
--       [ label [ for "host-backup-frequency" ] [ text "Backup frequency" ]
--       , input
--         [ id "host-backup-frequency"
--         , value <| withDefault "" host.backupFrequency
--         , disabled d
--         , onInput <| (\str -> OnModifyHostBackupFrequency datasetIdx distributionIdx (
--           case str of
--             "" -> Nothing
--             s -> Just s
--         ))
--         ]
--         []
--       ]
--     , div [ class "form-field" ]
--       [ label [ for "host-geo-location" ] [ text "Geo location" ]
--       , input
--         [ id "host-geo-location"
--         , value <| withDefault "" host.geoLocation
--         , disabled d
--         , onInput <| (\str -> OnModifyHostGeoLocation datasetIdx distributionIdx (
--           case str of
--             "" -> Nothing
--             s -> Just s
--         ))
--         ]
--         []
--       ]
--     ]
--   ]
-- 
-- distributionEditorView : Distribution -> Int -> Int -> Bool -> Html Msg
-- distributionEditorView distribution datasetIdx distributionIdx d = div [ class "dmp-editor-distribution" ]
--   [ h4 [] [ text <| "Distribution " ++ String.fromInt distributionIdx ]
--   , div [ class "dmp-editor-distribution-fields" ]
--     [ div [ class "form-field" ]
--       [ label [ for "distribution-data-access" ] [ text "Data access" ]
--       , select
--         [ id "distribution-data-access"
--         , value <| case distribution.dataAccess of
--           Open -> "Open"
--           Shared -> "Shared"
--           Closed -> "Closed"
--         , disabled d
--         , onInput <| (\str -> OnModifyDistributionDataAccess datasetIdx distributionIdx (
--           case str of
--             "Shared" -> Shared
--             "Closed" -> Closed
--             _ -> Open
--         ))
--         ]
--         -- There's an elm bug or something where the initial value attribute is not being updated
--         -- correctly. So we are setting the redundant `selected` attribute as well.
--         [ option [ value "Open", selected <| distribution.dataAccess == Open ] [ text "Open" ]
--         , option [ value "Shared", selected <| distribution.dataAccess == Shared ] [ text "Shared" ]
--         , option [ value "Closed", selected <| distribution.dataAccess == Closed ] [ text "Closed" ]
--         ]
--       ]
--     , div [ class "form-field" ]
--       [ label [ for "distribution-access-url" ] [ text "Access url" ]
--       , input
--         [ id "distribution-access-url"
--         , value <| withDefault "" distribution.accessUrl
--         , disabled d
--         , onInput <| (\str -> OnModifyDistributionAccessUrl datasetIdx distributionIdx (
--           case str of
--             "" -> Nothing
--             s -> Just s
--         ))
--         ]
--         []
--       ]
--     ]
--   , case distribution.host of
--     Just host -> hostEditorView host datasetIdx distributionIdx d
--     Nothing -> text ""
--   , button
--     [ onClick <| OnToggleHost datasetIdx distributionIdx
--     , disabled d
--     , class <| if distribution.host == Nothing then "btn" else "btn btn-danger"
--     ]
--     [ text <| if distribution.host == Nothing then "+ Add Host" else "- Remove Host" ]
--   , hr [] []
--   , button
--     [ onClick <| OnRemoveDistribution datasetIdx distributionIdx
--     , disabled d
--     , class "btn btn-danger"
--     ]
--     [ text "- Remove distribution" ]
--   ]
-- 
-- datasetEditorView : Dataset -> Int -> Bool -> Html Msg
-- datasetEditorView dataset datasetIdx d = div [ class "dmp-editor-dataset" ]
--   [ h3 [] [ text <| "Dataset " ++ String.fromInt datasetIdx ]
--   , div [ class "form-field" ]
--     [ label [ for "dataset-editor-title" ] [ text "Title" ]
--     , input
--       [ id "dataset-editor-title"
--       , value dataset.title
--       , disabled d
--       , onInput <| OnModifyDatasetTitle datasetIdx
--       ]
--       []
--     ]
--   , div [ class "form-field" ]
--     [ label [ for "dataset-editor-personal-data" ] [ text "Personal Data" ]
--     , select
--       [ id "distribution-editor-personal-data"
--       , value <| case dataset.personalData of
--         Yes -> "Yes"
--         No -> "No"
--         Unknown -> "Unknown"
--       , disabled d
--       , onInput <| (\str -> OnModifyDatasetPersonalData datasetIdx (
--         case str of
--           "Yes" -> Yes
--           "No" -> No
--           _ -> Unknown
--       ))
--       ]
--       [ option [ value "Yes", selected <| dataset.personalData == Yes ] [ text "Yes" ]
--       , option [ value "No", selected <| dataset.personalData == No ] [ text "No" ]
--       , option [ value "Unknown", selected <| dataset.personalData == Unknown ] [ text "Unknown" ]
--       ]
--     ]
--   , div []
--     [ div [ class "dmp-editor-distribution-list" ]
--       <| Array.toList
--       <| Array.indexedMap (\distributionIdx dist -> distributionEditorView dist datasetIdx distributionIdx d) dataset.distributions
--     , button
--       [ onClick <| OnAddDistribution datasetIdx
--       , disabled d
--       , class "btn"
--       ]
--       [ text "+ Add distribution" ]
--     , hr [] []
--     ]
--   , button
--     [ onClick <| OnRemoveDataset datasetIdx
--     , disabled d
--     , class "btn btn-danger"
--     ]
--     [ text "- Remove dataset" ]
--   ]

dmpEditorView : Dmp -> Bool -> EditorMode -> User.LoginSession -> Html Msg
dmpEditorView dmp d mode session =
  let
    orgToOption org = option [ value org, selected <| dmp.dmpOrgId == org ] [ text org ]
  in div [ class "dmp-editor" ]
    [ h2 [] [ text "Edit DMP" ]
    , div [ class "form-field" ] <| case mode of
      Edit _ -> [ label [] [ text <| "Organization: " ++ dmp.dmpOrgId ] ]
      New ->
        case session of
          User.LoggedIn personToken person ->
            [ label [ for "dmp-editor-org" ] [ text "Organization" ]
            , select
              [ id "dmp-editor-org"
              , value <| case Array.get 0 person.organisation of
                Just org -> org
                Nothing -> ""
              , disabled d
              , onInput (\str -> OnModifyDmpOrg str)
              ]
              (Array.toList <| Array.map orgToOption person.organisation)
            ]
          _ -> [ text "You have to be logged in to use the DMP editor." ]
--    , div [ class "dmp-editor-dataset-list" ]
--      <| Array.toList
--      <| Array.indexedMap (\idx ds -> datasetEditorView ds idx d) dmp.datasets
--    , button
--      [ onClick OnAddDataset
--      , disabled d
--      , class "btn"
--      ]
--      [ text "+ Add dataset" ]
    , hr [] []
    ]

editorFormView : Model -> Html Msg
editorFormView model = 
  div [ class "dmp-editor-wrapper" ]
  [ div []
      [ dmpEditorView model.dmp (model.status == Submitting) model.mode model.session
      , button
        [ onClick OnSubmit
        , disabled (model.status == Submitting)
        , class "btn btn-primary"
        ]
        [ text "Submit" ]
      ]
  , div [] <| case model.status of
    SubmitError e -> [ text <| "Error submitting DMP: " ++ Debug.toString e ]
    NotLoggedInError -> [ text <| "Error submitting DMP: Not logged in!" ]
    _ -> []
  ]

view : Model -> Html Msg
view model = editorFormView model
