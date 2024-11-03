module Pages.DmpNew exposing (..)

import Browser
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

type ModelStatus = Editing | Submitting | SubmitError Error

type alias Model =
  { dmp: DataManagementPlan
  , status: ModelStatus
  , key: Nav.Key
  }

type Msg = OnTestFieldChange String | OnSubmit | GotNewDmpResponse (Result Error String)

init : Nav.Key -> ( Model, Cmd Msg )
init key = ({ dmp = { id = Nothing, testField = "" }, status = Editing, key = key }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnTestFieldChange str ->
      let updateDmp dmp = { dmp | testField = str }
      in ( { model | dmp = updateDmp model.dmp }, Cmd.none )
    OnSubmit -> case model.status of
      Submitting -> (model, Cmd.none)
      Editing -> ({ model | status = Submitting }, newDmp model.dmp GotNewDmpResponse)
      SubmitError e -> ({ model | status = Submitting }, newDmp model.dmp GotNewDmpResponse)
    GotNewDmpResponse res -> case res of
      Ok str -> 
        (model, Nav.pushUrl model.key "/dmp")
      Err e ->
        ({ model | status = SubmitError e }, Cmd.none)

dmpEditorView : Model -> Html Msg
dmpEditorView model = 
  div []
  [ div []
      [ input
        [ value model.dmp.testField
        , onInput OnTestFieldChange
        , disabled (model.status == Submitting)
        ]
        []
      , button
        [ onClick OnSubmit
        , disabled (model.status == Submitting)
        ]
        [ text "Submit" ]
      ]
  , div [] <| case model.status of
    SubmitError e -> [ text <| "Error submitting DMP: " ++ Debug.toString e ]
    _ -> []
  ]

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp New View"
  , body = dmpEditorView model
  }
