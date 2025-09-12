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
import Html.Attributes
import Json.Decode.Pipeline
import Config exposing (Config)
import Utils exposing (httpErrorToString)

port updateLocalStorage : Json.Encode.Value -> Cmd msg
port toggleDialog : String -> Cmd msg

type alias Model =
  { key: Nav.Key
  , loginSession: LoginSession
  , routeModel: RouteModel
  , currentRoute: Maybe Route
  , config: Config
  }

type RouteModel
  = NoModel
  | ErrorModel String
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

unwrapMaybeRoute route = case route of
  Just r -> r
  Nothing -> FrontRoute

type alias Flags =
  { maybeLogin : Maybe String
  , dmpApiBase : String
  , lajiApiBase : String
  , authUrl : String
  }

decodeFlags : Json.Decode.Value -> Result Json.Decode.Error Flags
decodeFlags flags = 
  let
    decoder = 
      Json.Decode.succeed Flags
      |> Json.Decode.Pipeline.optional "login" (Json.Decode.nullable Json.Decode.string) Nothing
      |> Json.Decode.Pipeline.required "dmpApiBase" Json.Decode.string
      |> Json.Decode.Pipeline.required "lajiApiBase" Json.Decode.string
      |> Json.Decode.Pipeline.required "authUrl" Json.Decode.string
  in Json.Decode.decodeValue decoder flags

parseFlags : Json.Decode.Value -> Flags
parseFlags flags = case decodeFlags flags of
  Err err -> Flags Nothing "http://localhost:4000" "https://dev.laji.fi/api/" "https://fmnh-ws-test.it.helsinki.fi/laji-auth/login?target=KE.1661&redirectMethod=GET&locale=fi&next="
  Ok f -> f

init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsJson url key =
  let
    flags = parseFlags flagsJson
    cfg = Config flags.dmpApiBase flags.lajiApiBase flags.authUrl
    route = fromUrl url
  in
    case flags.maybeLogin of
      Nothing -> changeRouteTo route
        { key = key
        , routeModel = NoModel
        , loginSession = NotLoggedIn
        , currentRoute = route
        , config = cfg
        }
      Just token -> Tuple.mapSecond
        (\c -> Cmd.batch [ c, User.getPerson cfg token <| GotPerson token ])
        ( changeRouteTo (fromUrl url)
          { key = key
          , routeModel = NoModel
          , loginSession = LoadingPerson token
          , currentRoute = route
          , config = cfg
          }
        )

changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo maybeRoute model =
  let
    mapPageInit : (m -> RouteModel) -> (c -> Msg) -> (m, Cmd c) -> (Model, Cmd Msg)
    mapPageInit newRouteModel newMsg initFn = Tuple.mapBoth (\m -> { model | routeModel = newRouteModel m, currentRoute = maybeRoute}) (\c -> Cmd.map newMsg c) initFn
  in
    case maybeRoute of
      Nothing -> ( model, Cmd.none )
      Just FrontRoute -> mapPageInit FrontModel GotFrontMsg Pages.Front.init
      Just (DmpRoute dmpRoute) -> case dmpRoute of
        DmpIndexRoute -> mapPageInit DmpIndexModel GotDmpIndexMsg <| Pages.DmpIndex.init model.config model.loginSession
        DmpNewRoute -> mapPageInit DmpNewModel GotDmpNewMsg <| Pages.DmpNew.init model.key model.loginSession
        DmpElementRoute dmpElementRoute -> case dmpElementRoute of
          (DmpInfoRoute id) -> mapPageInit DmpInfoModel GotDmpInfoMsg <| Pages.DmpInfo.init model.config id model.loginSession
          (DmpEditRoute id) -> mapPageInit DmpEditModel GotDmpEditMsg <| Pages.DmpEdit.init model.config model.key id model.loginSession
      Just (LoginRoute maybeToken maybeNext) ->
        case (maybeToken, maybeNext) of
          (Just token, next) ->
            ( { model | loginSession = LoadingPerson token }
            , Cmd.batch 
              [ Nav.pushUrl model.key <| case next of
                Just n -> if String.length n > 0 then n else "/"
                Nothing -> "/"
              , User.getPerson model.config token <| GotPerson token
              ]
            )
          (_, _) -> ( model, Nav.pushUrl model.key "/" )

updateSession : LoginSession -> RouteModel -> RouteModel
updateSession newSession routeModel =
  case routeModel of
    DmpIndexModel model -> DmpIndexModel { model | session = newSession }
    DmpInfoModel model -> DmpInfoModel { model | session = newSession }
    DmpNewModel model -> DmpNewModel { model | session = newSession }
    DmpEditModel model -> DmpEditModel { model | session = newSession }
    a -> a

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
        mapPageUpdate DmpInfoModel GotDmpInfoMsg (Pages.DmpInfo.update model.loginSession subMsg mod)
      (GotDmpEditMsg subMsg, DmpEditModel mod) ->
        case subMsg of
          Pages.DmpEdit.OnDelete -> (model, toggleDialog Pages.DmpEdit.deleteDialogId)
          Pages.DmpEdit.OnCancelDelete -> (model, toggleDialog Pages.DmpEdit.deleteDialogId)
          _ -> mapPageUpdate DmpEditModel GotDmpEditMsg (Pages.DmpEdit.update model.config subMsg mod)
      (GotDmpNewMsg subMsg, DmpNewModel mod) ->
        mapPageUpdate DmpNewModel GotDmpNewMsg (Pages.DmpNew.update model.config subMsg mod)
      (GotPerson token res, _) ->
        case res of
          Ok person ->
            let session = LoggedIn token person
            in ( { model | loginSession = session, routeModel = updateSession session model.routeModel }, updateLocalStorage <| User.encodeLogin session )
          Err e ->
            ({ model | routeModel = ErrorModel <| String.append "Unable to get person: " <| httpErrorToString e, loginSession = NotLoggedIn }, updateLocalStorage <| User.encodeLogin NotLoggedIn)
      (OnDeleteToken token, _) ->
        ({ model | loginSession = DeletingToken token }, User.deleteToken model.config token DeletedToken)
      (DeletedToken res, _) ->
        case res of
          Ok str ->
            let session = NotLoggedIn
            in ( { model | loginSession = session, routeModel = updateSession session model.routeModel }, updateLocalStorage <| User.encodeLogin session)
          Err e ->
            ({ model | routeModel = ErrorModel <| String.append "Unable to delete person token: " <| httpErrorToString e }, Cmd.none)
      (_, _) -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  let
    viewPage toMsg subView =
      { title = subView.title, body =
        [ Html.div [Html.Attributes.class "main"]
          [ Views.Navigation.navigation model.config model.loginSession model.currentRoute OnDeleteToken
          , Html.map (\msg -> toMsg msg) subView.body
          ]
        ]
      }
  in
    case model.routeModel of
      NoModel -> { title = "", body = [] }
      ErrorModel e -> viewPage GotFrontMsg <| { title = "Error", body = text <| "Error: " ++ e }
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
