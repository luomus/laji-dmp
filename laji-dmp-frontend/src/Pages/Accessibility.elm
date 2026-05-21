module Pages.Accessibility exposing (..)

import Browser
import Html.Attributes exposing (href)
import Html exposing (a, p, ul, li)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (Html)

type alias Model = {}

type Msg = Empty

init : ( Model, Cmd Msg )
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Saavutettavuusseloste - Luonto DMP"
  , body =
      Html.div []
        [ Html.h1 [] [ text "Saavutettavuusseloste" ]
        , Html.p [] [ text "Tämä on luonnontieteellisen keskusmuseon saavutettavuusseloste koskien palvelua Luonto-DMP. Palvelua koskee laki digitaalisten palveluiden tarjoamisesta, joka velvoittaa noudattamaan ", Html.a [ href "https://www.saavutettavuusvaatimukset.fi/fi/wcag-21-lain-vaatimukset", Html.Attributes.target "_blank"] [ text "WCAG 2.1 ohjeistuksen tasojen A ja AA kriteerejä." ] ]
        , Html.p []
          [ text "Alla kuvaamme palvelun keskeisimmät tiedossa olevat puutteet WCAG 2.1 ohjeistukseen verraten:"
          , ul []
            [ li [] [ text "3.1.1; Sivun kieli ei ole ohjelmallisesti saavutettavissa" ]
            , li [] [ text "1.3.1; 3.3.1; Lomakkeiden aria atribuutit puuttuvat" ]
            , li [] [ text "1.4.10; Sivuston rakentamisessa ei ole merkittävästi otettu huomioon mobiilisaavutettavuutta" ]
            , li [] [ text "4.1.3; HTML-elementtien rooleja ei voi aina selvittää ohjelmallisesti" ]
            ]
          ]
        , Html.h2 [] [ text "Saavutettavuuspalaute" ]
        , p [] [ text "Huomasitko saavutettavuuspuutteen digipalvelussamme? Kerro se meille ja teemme parhaamme puutteen korjaamiseksi. Voit lähettää palautteen sähköpostilla osoitteeseen ", a [ href "mailto:helpdesk@laji.fi" ] [ text "helpdesk@laji.fi" ] ]
        , Html.h2 [] [ text "Valvontaviranomainen" ]
        , p [] [ text "Jos huomaat sivustolla saavutettavuusongelmia, anna ensin palautetta meille eli sivuston ylläpitäjälle. Vastauksessa voi mennä 14 päivää. Jos et ole tyytyväinen saamaasi vastaukseen tai et saa vastausta lainkaan kahden viikon aikana, voit tehdä ilmoituksen Etelä-Suomen aluehallintovirastoon. Etelä-Suomen aluehallintoviraston sivulla kerrotaan tarkasti, miten ilmoituksen voi tehdä ja miten asia käsitellään." ]
        ]
  }
