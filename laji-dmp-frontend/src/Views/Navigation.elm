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

navigation: Html msg
navigation = 
  nav [ class "main-nav" ]
    [ h1 [class "nav-title"] [text "Data Management Plan Tool"]
    , ul []
      [ li [] [a [href "/", class "nav-link"] [text "Front"]]
      , li [] [a [href "/dmp", class "nav-link"] [text "Index"]]
      , li [] [a [href "/login", class "nav-link"] [text "Login"]]
      ]
    ]
