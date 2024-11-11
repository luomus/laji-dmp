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

loginView : LoginSession -> (String -> msg) -> Html msg
loginView loginSession deleteMsg =
  div [class "login-container"]
    [ case loginSession of
      LoggedIn token person -> div [class "logged-in"]
        [ text <| person.fullName ++ " (" ++ person.id ++ ")"
        , Html.button [ Html.Events.onClick <| deleteMsg token] [Html.text "Log out"]
        ]
      NotLoggedIn -> Html.text "Not logged in"
      LoadingPerson token -> Html.text "Logging in..."
      DeletingToken token -> Html.text "Logging out..."
    ]

navigation : LoginSession -> Maybe Route -> (String -> msg) -> Html msg
navigation loginSession currentRoute deleteMsg = 
  let
    getLinkAttribs : String -> List (Html.Attribute msg)
    getLinkAttribs url =
      let
        activeLinkAttribs = [href url, class "nav-link nav-link-active"]
        defaultLinkAttribs = [href url, class "nav-link"]
      in case currentRoute of
        Just definitelyCurrentRoute -> case (Url.fromString <| "http://0.0.0.0" ++ url) of
          Just definitelyUrl -> case fromUrl definitelyUrl of
            Just route -> case (route, definitelyCurrentRoute) of
              (FrontRoute, FrontRoute) -> activeLinkAttribs
              (DmpRoute _, DmpRoute _) -> activeLinkAttribs
              (_, _) -> defaultLinkAttribs
            Nothing -> Debug.log "Invalid path" defaultLinkAttribs
          Nothing -> Debug.log "Invalid url" defaultLinkAttribs
        Nothing -> Debug.log "Invalid current route" defaultLinkAttribs
  in
    nav [ class "main-nav" ]
      [ h1 [class "nav-title"] [text "Data Management Plan Tool"]
      , div [class "navbar"]
        [ ul []
          [ li [] [a (getLinkAttribs "/") [text "About"]]
          , li [] [a (getLinkAttribs "/dmp") [text "Index"]]
          ]
        , loginView loginSession deleteMsg
        ]
      ]
