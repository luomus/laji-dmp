module Pages.Front exposing (..)

import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
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
  { title = "Etusivu"
  , body =
      Html.div []
        [ Html.h1 [] [ text "Käyttöohjeet" ]
        , Html.p [] [ text "Luonto-DMP sovelluksella voit hallita aineistonhallintasuunnitelmia (DMP). Sovelluksella voi tarkastella kaikkia DMP:itä kirjautumatta, mutta DMP:n luominen, muokkaaminen ja poistaminen vaatii kirjautumisen Lajitietokeskus-tilillä. Lisäksi Lajitietokeskus-tili tulee liittää organisaatioon ennen kirjautumista sovellukseen." ]
        , Html.h2 [] [ text "Kirjautuminen" ]
        , Html.p [] [ text "Jos sinulla ei ole Lajitietokeskus-tiliä, klikkaa ensin oikeasta yläkulmasta 'Kirjaudu' painiketta. Voit joko luoda erillisen Laji.fi käyttäjätunnuksen ja salasanan 'Luo tunnus' painikkeesta tai luoda tunnuksen muulla kirjautumistavalla, esim. Haka, Google tai Facebook." ]
        , Html.p [] [ text "Ennen kuin kirjaudut Lajitietokeskus-tilillä sovellukseen, tilisi tulee liittää organisaatioon. Jos organisaatiosi käyttää Viranomaisportaalia, pyydä organisaatiosi admin-käyttäjää lisäämään sinut organisaatioon Viranomaisportaalissa: ", Html.a [ Html.Attributes.href "https://viranomaiset.laji.fi" ] [ text "viranomaiset.laji.fi" ], text ". Admin-käyttäjä navigoi Viranomaisportaalissa 'Aineistojen käyttö' -välilehden 'Käyttäjänhallinta' -sivulle ja lisää sieltä käyttäjäsi organisaatioon." ]
        , Html.p [] [ text "Jos organisaatiosi ei käytä Viranomaisportaalia, lähetä viestiä osoitteeseen ", Html.a [ Html.Attributes.href "mailto:helpdesk@laji.fi" ] [ text "helpdesk@laji.fi" ], text " ja ilmoita, mihin organisaatioon kuulut. Odota vastausta ja sitten voit kirjautua sovellukseen." ]
        , Html.h2 [] [ text "DMP:n luominen" ]
        , Html.p [] [ text "Voit luoda uuden DMP:n navigoimalla 'DMP-luettelo' välilehdelle ja painamalla '+ Uusi DMP' painiketta. Tästä avautuu lomakenäkymä, johon täytetään tarvittavat tiedot." ]
        , Html.p [] [ text "DMP:n rakenne on hierarkinen, jolloin eri osioita voi lisätä useita. Esimerkiksi yhdelle DMP:lle voi lisätä useamman aineiston ja yhdelle aineistolle useampia muita, esim. metatiedoista kertovia kenttiä." ]
        , Html.p [] [ text "Kaikkia tietoa ei ole pakko täyttää, vaan ainoastaan * -merkillä merkityt. Myös vapaaehtoisten osioiden alla on  * -merkittyjä kenttiä, jotka on pakko täyttää vain, jos lisäät kyseisen osion. Jos yrität tallentaa lomaketta ilman näiden kohtien täyttämistä, tallennus ei onnistu. Tällöin palaa takaisin täyttämään puuttuvat kentät ja tallenna lomake uudestaan." ]
        , Html.p [] [ text "Tallennuksen jälkeen DMP tulee kaikille käyttäjille näkyviin DMP-luetteloon, mutta on muokattavissa ja poistettavissa vain organisaatiosi jäsenille. Huomaa, että organisaatiolle voi halutessaan lisätä useita DMP:itä." ]
        , Html.p [] [ text "Yksittäisen DMP:n näkymään pääsee klikkaamalla DMP:tä luettelosta. DMP:n sivulta löytyy 'Muokkaa' -painike ja muokkauslomakkeelta löytyy 'Poista' -painike." ]
        , Html.h2 [] [ text "Rajapinta" ]
        , Html.p [] [ text "Sovelluksen rajapintaa (API) voi käyttää API-välilehdeltä. Näin pääset Swagger-sivulle, jossa voit suorittaa kyselyjä sovelluksen rajapintaan, joka palauttaa JSON-muotoista dataa." ]
        , Html.p [] [ text "GET -endpointit eivät vaadi kirjautumista. Voit siis hakea joko kaikki DMP:t GET /dmp -endpointista tai hakea tiettyä DMP:tä sen tunnisteella GET /dmp/{id} -endpointista. Muihin endpointteihin tarvitset Lajitietokeskus-tilisi personTokenin. PersonTokenin avulla voit lähettää DMP:itä POST /dmp -endpointtiin, poistaa oman organisaation DMP:itä DELETE /dmp/{id} -endpointilla ja muokata oman organisaation DMP:itä PUT /dmp/{id} -endpointilla. Tämä tekee rajapinnasta täysin koneluettavan ja -käytettävän." ]
        , Html.h2 [] [ text "Ongelmatilanteet" ]
        , Html.p [] [ text "Ongelmatilanteissa ota yhteyttä ", Html.a [ Html.Attributes.href "mailto:helpdesk@laji.fi" ] [ text "helpdesk@laji.fi" ]]
        , Html.p [] [ text "2025 Luomus" ]
        ]
  }
