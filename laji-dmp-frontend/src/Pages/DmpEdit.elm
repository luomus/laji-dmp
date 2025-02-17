module Pages.DmpEdit exposing (..)

import Html exposing (Html)
import Browser.Navigation as Nav
import Views.DmpEditor exposing (EditorMode(..))
import Html exposing (button)
import Html exposing (text)
import Html.Events exposing (onClick)
import Views.DmpEditor exposing (ModelStatus(..))
import DmpApi exposing (deleteDmp)
import Http exposing (Error)
import Views.Dialog exposing (dialog)
import DmpApi exposing (DataManagementPlan)
import DmpApi exposing (getDmp)
import Html.Attributes exposing (class)
import User

type EditorState
  = EditorModel Views.DmpEditor.Model
  | Loading Nav.Key String
  | Error

type alias Model =
  { state: EditorState
  , session: User.LoginSession
  }

type Msg
  = EditorMsg Views.DmpEditor.Msg
  | OnDelete
  | OnConfirmDelete
  | OnCancelDelete
  | GotDmpDeleteResponse (Result Error String)
  | GotDmpGetResponse (Result Http.Error DataManagementPlan)

deleteDialogId : String
deleteDialogId = "delete-dialog"

init : Nav.Key -> String -> User.LoginSession -> ( Model, Cmd Msg )
init key strId session =
  let maybeId = String.toInt strId
  in case maybeId of
    Just id ->
      ({ state = Loading key strId, session = session }, getDmp id GotDmpGetResponse)
    Nothing -> ({ state = Error, session = session }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model.state) of
  (EditorMsg subMsg, EditorModel subModel) -> Tuple.mapBoth (\m -> { model | state = EditorModel m }) (Cmd.map EditorMsg) <| Views.DmpEditor.update subMsg subModel
  (OnDelete, EditorModel _) -> (model, Cmd.none)
  (OnCancelDelete, EditorModel _) -> (model, Cmd.none)
  (OnConfirmDelete, EditorModel subModel) ->
    ( { model | state = EditorModel { subModel | status = Submitting } }
    , case subModel.mode of
      Edit id -> case model.session of
        User.LoggedIn personToken person -> deleteDmp id personToken GotDmpDeleteResponse
        _ -> Debug.log "Error: tried to delete DMP while not logged in" Cmd.none
      _ -> Debug.log "Error: tried to delete DMP while not in edit mode" Cmd.none
    )
  (GotDmpDeleteResponse res, EditorModel subModel) -> case res of
    Ok str -> 
      (model, Nav.pushUrl subModel.key "/dmp")
    Err e ->
      ({ model | state = EditorModel { subModel | status = SubmitError e }}, Cmd.none)
  (GotDmpGetResponse res, Loading key strId) ->
    case res of
      Ok dmp ->
        (
          { model | state = EditorModel
            { dmp = dmp
            , session = model.session
            , status = Editing
            , key = key
            , mode = Edit strId
            }
          }
        , Cmd.none
        )
      Err e ->
        let _ = Debug.log "Error loading DMP" e
        in ({ model | state = Error }, Cmd.none)
  (_, _) -> ({ model | state = Error }, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "DMP Edit View"
  , body = Html.div [] <| case model.state of
    EditorModel subModel ->
      [ Html.map EditorMsg <| Views.DmpEditor.view subModel
      , button [ onClick OnDelete, class "btn btn-danger" ] [text "Delete DMP"]
      , dialog deleteDialogId []
        [ text "Do you really want to delete the DMP?"
        , button [ onClick OnConfirmDelete ] [text "Delete DMP"]
        , button [ onClick OnCancelDelete ] [text "Cancel"]
        ]
      ]
    Loading key id -> [text "Loading DMP..."]
    Error -> [text "Error"]
  }
