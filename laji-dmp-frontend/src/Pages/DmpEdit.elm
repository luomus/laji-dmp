module Pages.DmpEdit exposing (..)

import Html exposing (Html)
import Browser.Navigation as Nav
import Views.DmpEditor exposing (EditorMode(..))
import Html exposing (button)
import Html exposing (text)
import Html.Events exposing (onClick)
import Views.DmpEditor exposing (ModelStatus(..))
import Http
import DmpApi exposing (deleteDmp)
import Views.Dialog exposing (dialog)
import DmpApi exposing (getDmp)
import Models exposing (Dmp)
import Html.Attributes exposing (class)
import User
import Config exposing (Config)
import Utils exposing (httpErrorToString)
import User exposing (LoginSession(..))
import DmpApi exposing (ErrorResponse)

type EditorState
  = EditorModel Views.DmpEditor.Model
  | Loading Nav.Key String
  | Error String

type alias Model =
  { state: EditorState
  , session: User.LoginSession
  }

type Msg
  = EditorMsg Views.DmpEditor.Msg
  | OnDelete
  | OnConfirmDelete
  | OnCancelDelete
  | GotDmpDeleteResponse (Result ErrorResponse String)
  | GotDmpGetResponse (Result Http.Error Dmp)

deleteDialogId : String
deleteDialogId = "delete-dialog"

init : Config -> Nav.Key -> String -> User.LoginSession -> ( Model, Cmd Msg )
init cfg key strId session =
  let maybeId = String.toInt strId
  in case maybeId of
    Just id ->
      ({ state = Loading key strId, session = session }, getDmp cfg id GotDmpGetResponse)
    Nothing -> ({ state = Error "No id provided", session = session }, Cmd.none)

update : Config -> Msg -> Model -> (Model, Cmd Msg)
update cfg msg model = case (msg, model.state) of
  (EditorMsg subMsg, EditorModel subModel) -> Tuple.mapBoth (\m -> { model | state = EditorModel m }) (Cmd.map EditorMsg) <| Views.DmpEditor.update cfg subMsg subModel
  (OnDelete, EditorModel _) -> (model, Cmd.none)
  (OnCancelDelete, EditorModel _) -> (model, Cmd.none)
  (OnConfirmDelete, EditorModel subModel) ->
    case subModel.mode of
      Edit id -> case model.session of
        User.LoggedIn personToken person ->
          ( { model | state = EditorModel { subModel | status = Submitting } }
            , deleteDmp cfg id personToken GotDmpDeleteResponse
          )
        _ -> ({ model | state = Error "Tried to delete DMP while not logged in" }, Cmd.none)
      _ -> ({ model | state = Error "Tried to delete DMP while not in edit mode" }, Cmd.none)
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
        ({ model | state = Error <| "Error loading DMP response: " ++ httpErrorToString e }, Cmd.none)
  (_, _) -> ({ model | state = Error "Error loading DMP" }, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "DMP:n muokkaus"
  , body = case model.session of
    LoggedIn token person ->
      Html.div [] <| case model.state of
      EditorModel subModel ->
        [ Html.map EditorMsg <| Views.DmpEditor.view subModel
        , button [ onClick OnDelete, class "btn btn-danger" ] [text "Poista DMP"]
        , dialog deleteDialogId []
          [ text "Haluatko varmasti poistaa DMP:n?"
          , button [ onClick OnConfirmDelete ] [text "Poista DMP"]
          , button [ onClick OnCancelDelete ] [text "Peruuta"]
          ]
        ]
      Loading key id -> [text "Ladataan DMP:tä..."]
      Error err -> [text <| "Virhe: " ++ err]
    LoadingPerson token -> Html.div [] [ text "Logging in..." ]
    _ -> Html.div [] [ text "Error: not logged in!" ]
  }
