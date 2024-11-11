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

type Model
  = EditorModel Views.DmpEditor.Model
  | Loading Nav.Key String
  | Error

type Msg
  = EditorMsg Views.DmpEditor.Msg
  | OnDelete
  | OnConfirmDelete
  | OnCancelDelete
  | GotDmpDeleteResponse (Result Error String)
  | GotDmpGetResponse (Result Http.Error DataManagementPlan)

deleteDialogId : String
deleteDialogId = "delete-dialog"

init : Nav.Key -> String -> ( Model, Cmd Msg )
init key strId =
  let maybeId = String.toInt strId
  in case maybeId of
    Just id ->
      (Loading key strId, getDmp id GotDmpGetResponse)
    Nothing -> (Error, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (EditorMsg subMsg, EditorModel subModel) -> Tuple.mapBoth (\m -> EditorModel m) (Cmd.map EditorMsg) <| Views.DmpEditor.update subMsg subModel
  (OnDelete, EditorModel _) -> (model, Cmd.none)
  (OnCancelDelete, EditorModel _) -> (model, Cmd.none)
  (OnConfirmDelete, EditorModel subModel) ->
    ( EditorModel { subModel | status = Submitting }
    , case subModel.mode of
      Edit id -> deleteDmp id GotDmpDeleteResponse
      _ -> Debug.log "Error: tried to delete DMP while not in edit mode" Cmd.none
    )
  (GotDmpDeleteResponse res, EditorModel subModel) -> case res of
    Ok str -> 
      (model, Nav.pushUrl subModel.key "/dmp")
    Err e ->
      (EditorModel { subModel | status = SubmitError e }, Cmd.none)
  (GotDmpGetResponse res, Loading key strId) ->
    case res of
      Ok dmp ->
        ( EditorModel
          { dmp = dmp
          , status = Editing
          , key = key
          , mode = Edit strId
          }
        , Cmd.none
        )
      Err e ->
        let _ = Debug.log "Error loading DMP" e
        in (Error, Cmd.none)
  (_, _) -> (Error, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "DMP Edit View"
  , body = Html.div [] <| case model of
    EditorModel subModel ->
      [ Html.map EditorMsg <| Views.DmpEditor.view subModel
      , button [ onClick OnDelete ] [text "Delete DMP"]
      , dialog deleteDialogId []
        [ text "Do you really want to delete the DMP?"
        , button [ onClick OnConfirmDelete ] [text "Delete DMP"]
        , button [ onClick OnCancelDelete ] [text "Cancel"]
        ]
      ]
    Loading key id -> [text "Loading DMP..."]
    Error -> [text "Error"]
  }
