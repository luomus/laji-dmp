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

type alias Model = Views.DmpEditor.Model

type Msg
  = EditorMsg Views.DmpEditor.Msg
  | OnDelete
  | OnConfirmDelete
  | OnCancelDelete
  | GotDmpDeleteResponse (Result Error String)

deleteDialogId : String
deleteDialogId = "delete-dialog"

init : Nav.Key -> String -> ( Model, Cmd Msg )
init key id = Tuple.mapSecond (Cmd.map EditorMsg) <| Views.DmpEditor.init key <| Edit id

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  EditorMsg subMsg -> Tuple.mapSecond (Cmd.map EditorMsg) <| Views.DmpEditor.update subMsg model
  OnDelete -> (model, Cmd.none)
  OnCancelDelete -> (model, Cmd.none)
  OnConfirmDelete ->
    ( { model | status = Submitting }
    , case model.mode of
      Edit id -> deleteDmp id GotDmpDeleteResponse
      _ -> Debug.log "Error: tried to delete DMP while not in edit mode" Cmd.none
    )
  GotDmpDeleteResponse res -> case res of
    Ok str -> 
      (model, Nav.pushUrl model.key "/dmp")
    Err e ->
      ({ model | status = SubmitError e }, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "DMP Edit View"
  , body = Html.div []
    [ Html.map EditorMsg <| Views.DmpEditor.view model
    , button [ onClick OnDelete ] [text "Delete DMP"]
    , dialog deleteDialogId []
      [ text "Do you really want to delete the DMP?"
      , button [ onClick OnConfirmDelete ] [text "Delete DMP"]
      , button [ onClick OnCancelDelete ] [text "Cancel"]
      ]
    ]
  }
