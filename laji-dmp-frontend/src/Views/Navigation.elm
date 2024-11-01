module Views.Navigation exposing (..)

import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Html exposing (Html)
import Html exposing (ul)
import Html exposing (li)

navigation: Html msg
navigation = 
  ul []
    [ li []
      [ a [href "/"] [text "DMP Tool"]
      , a [href "/dmp"] [text "DMP Index"]
      , a [href "/login"] [text "Login"]
      ]
    ]
