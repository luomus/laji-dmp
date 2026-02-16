module Views.Footer exposing (..)
import Html exposing (Html)
import Html exposing (div)
import Html exposing (text)
import Html exposing (img)
import Html.Attributes exposing (href)
import Html.Attributes exposing (src)
import Html.Attributes exposing (alt)
import Html exposing (hr)
import Html exposing (a)
import Html.Attributes exposing (class)
import Html.Attributes exposing (target)
import Html.Attributes exposing (height)

footerView : Html a
footerView = div []
    [ hr [] []
    , div [ class "footer-links" ]
        [ a [ href "https://info.laji.fi/etusivu/lajitietokeskus/tietosuojaseloste/", target "_blank" ] [ text "Tietosuoja ↗︎" ]
        -- , a [] [ text "Saavutettavuusseloste" ]
        ]
    , div [ class "footer-logos" ]
        [ img
            [ src "https://cdn.laji.fi/images/partners/priodiversity_logo.png"
            , alt "Priodiversity"
            , height 100] []
        , img
            [ src "https://cdn.laji.fi/images/partners/natura_2000.jpg"
            , alt "Natura 2000"
            , height 100] []
        , img
            [ src "https://cdn.laji.fi/images/partners/co_funded_by_eu_life.png"
            , alt "EU LIFE"
            , height 100] []
        ]
    ]

