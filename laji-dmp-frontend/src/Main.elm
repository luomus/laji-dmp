port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Url
import Json.Encode
import Json.Decode

import Routes exposing (..)
import Pages.Front
import Pages.DmpIndex
import Pages.DmpInfo
import Pages.DmpEdit
import Pages.DmpNew
import Http
import User exposing (LoginSession(..))
import Views.Navigation
import Html.Events

port updateLocalStorage : Json.Encode.Value -> Cmd msg

type alias Model =
  { key: Nav.Key
  , loginSession: LoginSession
  , routeModel: RouteModel
  }

type RouteModel
  = NoModel
  | FrontModel Pages.Front.Model
  | DmpIndexModel Pages.DmpIndex.Model
  | DmpInfoModel Pages.DmpInfo.Model
  | DmpEditModel Pages.DmpEdit.Model
  | DmpNewModel Pages.DmpNew.Model

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotFrontMsg Pages.Front.Msg
  | GotDmpIndexMsg Pages.DmpIndex.Msg
  | GotDmpInfoMsg Pages.DmpInfo.Msg
  | GotDmpEditMsg Pages.DmpEdit.Msg
  | GotDmpNewMsg Pages.DmpNew.Msg
  | GotPerson String (Result Http.Error User.PersonResponse)
  | OnDeleteToken String
  | DeletedToken (Result Http.Error String)

init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    maybeToken = User.decodeLogin flags
  in
    case maybeToken of
      Nothing -> changeRouteTo (fromUrl url) { key = key, routeModel = NoModel, loginSession = NotLoggedIn }
      Just token -> Tuple.mapSecond (\c -> Cmd.batch [ c, User.getPerson token <| GotPerson token ]) ( changeRouteTo (fromUrl url) { key = key, routeModel = NoModel, loginSession = LoadingPerson token } )

changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo maybeRoute model =
  let
    mapPageInit : (m -> RouteModel) -> (c -> Msg) -> (m, Cmd c) -> (Model, Cmd Msg)
    mapPageInit newRouteModel newMsg initFn = Tuple.mapBoth (\m -> { model | routeModel = newRouteModel m}) (\c -> Cmd.map newMsg c) initFn
  in
    case maybeRoute of
      Nothing -> ( model, Cmd.none )
      Just FrontRoute -> mapPageInit FrontModel GotFrontMsg Pages.Front.init
      Just DmpIndexRoute -> mapPageInit DmpIndexModel GotDmpIndexMsg Pages.DmpIndex.init
      Just (DmpInfoRoute id) -> mapPageInit DmpInfoModel GotDmpInfoMsg <| Pages.DmpInfo.init id
      Just (DmpEditRoute id) -> mapPageInit DmpEditModel GotDmpEditMsg <| Pages.DmpEdit.init model.key id
      Just DmpNewRoute -> mapPageInit DmpNewModel GotDmpNewMsg <| Pages.DmpNew.init model.key
      Just (LoginRoute maybeToken maybeNext) ->
        case (maybeToken, maybeNext) of
          (Just token, next) ->
            ( { model | loginSession = LoadingPerson token }
            , Cmd.batch 
              [ Nav.pushUrl model.key <| case next of
                Just n -> n
                Nothing -> "/"
              , User.getPerson token <| GotPerson token
              ]
            )
          (_, _) -> ( model, Nav.pushUrl model.key "/" )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    mapPageUpdate : (m -> RouteModel) -> (c -> Msg) -> (m, Cmd c) -> (Model, Cmd Msg)
    mapPageUpdate newRouteModel newMsg updateFn =
      Tuple.mapBoth (\m -> { model | routeModel = newRouteModel m }) (\c -> Cmd.map newMsg c) updateFn
  in
    case (msg, model.routeModel) of
      (LinkClicked urlRequest, _) ->
        case urlRequest of
          Browser.Internal url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )
          Browser.External href ->
            ( model, Nav.load href )
      (UrlChanged url, _) -> changeRouteTo (fromUrl url) model
      (GotFrontMsg subMsg, FrontModel mod) ->
        mapPageUpdate FrontModel GotFrontMsg (Pages.Front.update subMsg mod)
      (GotDmpIndexMsg subMsg, DmpIndexModel mod) ->
        mapPageUpdate DmpIndexModel GotDmpIndexMsg (Pages.DmpIndex.update subMsg mod)
      (GotDmpInfoMsg subMsg, DmpInfoModel mod) ->
        mapPageUpdate DmpInfoModel GotDmpInfoMsg (Pages.DmpInfo.update subMsg mod)
      (GotDmpEditMsg subMsg, DmpEditModel mod) ->
        mapPageUpdate DmpEditModel GotDmpEditMsg (Pages.DmpEdit.update subMsg mod)
      (GotDmpNewMsg subMsg, DmpNewModel mod) ->
        mapPageUpdate DmpNewModel GotDmpNewMsg (Pages.DmpNew.update subMsg mod)
      (GotPerson token res, _) ->
        case res of
          Ok person ->
            let session = LoggedIn token person
            in ( { model | loginSession = session }, updateLocalStorage <| User.encodeLogin session )
          Err e ->
            let _ = Debug.log "Unable to get person" e
            in ( { model | loginSession = NotLoggedIn }, updateLocalStorage <| User.encodeLogin NotLoggedIn)
      (OnDeleteToken token, _) ->
        ({ model | loginSession = DeletingToken token }, User.deleteToken token DeletedToken)
      (DeletedToken res, _) ->
        case res of
          Ok str ->
            let session = NotLoggedIn
            in ( { model | loginSession = session }, updateLocalStorage <| User.encodeLogin session)
          Err e ->
            let _ = Debug.log "Unable to delete person token" e
            in (model, Cmd.none)
      (_, _) -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  let
    viewPage toMsg subView =
      { title = subView.title, body =
        [ Views.Navigation.navigation
        , Html.div []
          [ case model.loginSession of
            LoggedIn token person -> Html.div []
              [ text <| person.id ++ " " ++ person.fullName
              , Html.button [ Html.Events.onClick <| OnDeleteToken token] [Html.text "Log out"]
              ]
            NotLoggedIn -> Html.text "Not logged in"
            LoadingPerson token -> Html.text "Logging in..."
            DeletingToken token -> Html.text "Logging out..."
          ]
        , Html.map (\msg -> toMsg msg) subView.body
        ]
      }
  in
    case model.routeModel of
      NoModel -> { title = "", body = [] }
      FrontModel subModel -> viewPage GotFrontMsg <| Pages.Front.view subModel
      DmpIndexModel subModel -> viewPage GotDmpIndexMsg <| Pages.DmpIndex.view subModel
      DmpInfoModel subModel -> viewPage GotDmpInfoMsg <| Pages.DmpInfo.view subModel
      DmpEditModel subModel -> viewPage GotDmpEditMsg <| Pages.DmpEdit.view subModel
      DmpNewModel subModel -> viewPage GotDmpNewMsg <| Pages.DmpNew.view subModel

main : Program Json.Decode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }
