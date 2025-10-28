module Views.Navigation exposing (..)

import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Html exposing (Html)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (nav)
import Html.Attributes exposing (class)
import Html exposing (h1)
import User exposing (LoginSession)
import User exposing (LoginSession(..))
import Html.Events
import Html exposing (div)
import Routes exposing (Route)
import Routes exposing (fromUrl)
import Url exposing (Url)
import Routes exposing (Route(..))
import Routes exposing (DmpSubRoute(..))
import Routes exposing (DmpElementSubRoute(..))
import Html exposing (span)
import Config exposing (Config)

loginView : Config -> LoginSession -> (String -> msg) -> Html msg
loginView cfg loginSession deleteMsg =
  div [class "login-container"]
    [ case loginSession of
      LoggedIn token person -> div [class "logged-in"]
        [ text <| person.fullName ++ " (" ++ person.id ++ ")"
        , Html.button [ class "btn", Html.Events.onClick <| deleteMsg token] [Html.text "Kirjaudu ulos"]
        ]
      NotLoggedIn -> a [ class "btn", href cfg.authUrl ] [ text "Kirjaudu" ]
      LoadingPerson token -> Html.text "Kirjaudutaan..."
      DeletingToken token -> Html.text "Kirjaudutaan ulos..."
    ]

breadcrumbs : Route -> Html msg
breadcrumbs currentRoute =
  let
    link url desc = a [href url, class "breadcrumbs-element"] [text desc]
    current desc = span [class "breadcrumbs-current"] [text desc]
    sep = span [class "breadcrumbs-separator"] [text ">"]
  in
    div [class "breadcrumbs"] <| case currentRoute of
      DmpRoute dmpRoute -> case dmpRoute of
        DmpIndexRoute -> [current "Luettelo"]
        DmpNewRoute -> [link "/dmp" "Luettelo", sep, current "Uusi DMP"]
        DmpElementRoute dmpElementRoute -> case dmpElementRoute of
          DmpInfoRoute id -> [link "/dmp" "Luettelo", sep, current <| "DMP " ++ id]
          DmpEditRoute id -> [link "/dmp" "Luettelo", sep, link ("/dmp/" ++ id) <| "DMP " ++ id, sep, current "Muokkaa"]
      _ -> []

urlFromPath : String -> Url
urlFromPath path =
  { protocol = Url.Http
  , host = ""
  , port_ = Nothing
  , path = if String.startsWith "/" path then path else "/" ++ path
  , query = Nothing
  , fragment = Nothing
  }

navigation : Config -> LoginSession -> Maybe Route -> (String -> msg) -> Html msg
navigation cfg loginSession maybeCurrentRoute deleteMsg = 
  let
    getLinkAttribs : String -> List (Html.Attribute msg)
    getLinkAttribs path =
      let
        activeLinkAttribs = [href path, class "nav-link nav-link-active"]
        defaultLinkAttribs = [href path, class "nav-link"]
      in case maybeCurrentRoute of
        Just currentRoute -> case fromUrl <| urlFromPath path of
          Just route -> case (route, currentRoute) of
            (FrontRoute, FrontRoute) -> activeLinkAttribs
            (DmpRoute _, DmpRoute _) -> activeLinkAttribs
            (_, _) -> defaultLinkAttribs
          Nothing -> []
        Nothing -> []
  in
    nav [ class "main-nav" ]
      [ h1 [class "nav-title"] [text "Luonto-DMP"]
      , div [class "navbar"]
        [ ul []
          [ li [] [a (getLinkAttribs "/") [text "Etusivu"]]
          , li [] [a (getLinkAttribs "/dmp") [text "DMP luettelo"]]
          , li [] [a [class "nav-link", href <| cfg.dmpApiBase ++ "/swagger-ui"] [text "API ↗︎"]]
          ]
        , loginView cfg loginSession deleteMsg
        ]
      , case maybeCurrentRoute of
        Nothing -> div [] []
        Just currentRoute -> case currentRoute of
          DmpRoute _ -> breadcrumbs currentRoute
          _ -> div [] []
      ]
