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
        [ Html.h2 [] [ text "Käyttöohjeet" ]
        , Html.p [] [ text "Luonto-DMP sovelluksella voit hallita aineistonhallintasuunnitelmia (DMP). Sovelluksella voi tarkastella kaikkia DMP:itä kirjautumatta, mutta DMP:n luominen, muokkaaminen ja poistaminen vaatii kirjautumisen Lajitietokeskus-tilillä. Lisäksi Lajitietokeskus-tili tulee liittää organisaatioon ennen kirjautumista sovellukseen." ]
        , Html.p [] [ text "Jos sinulla ei ole Lajitietokeskus-tiliä, klikkaa ensin oikeasta yläkulmasta 'Kirjaudu' painiketta. Voit joko luoda erillisen Laji.fi käyttäjätunnuksen ja salasanan 'Luo tunnus' painikkeesta tai luoda tunnuksen muulla kirjautumistavalla, esim. Haka, Google tai Facebook." ]
        , Html.p [] [ text "Ennen kuin kirjaudut Lajitietokeskus-tilillä sovellukseen, tilisi tulee liittää organisaatioon. Lähetä viestiä osoitteeseen ", Html.a [ Html.Attributes.href "mailto:helpdesk@laji.fi" ] [ text "helpdesk@laji.fi" ], text " ja ilmoita, mihin organisaatioon kuulut. Odota vastausta ja sitten voit kirjautua sovellukseen." ]
        , Html.p [] [ text "Kirjautuaksesi sisään, klikkaa oikeasta yläkulmasta 'Kirjaudu' painiketta, josta näet kaikki mahdolliset kirjautumistavat. Kirjaudu sisään jollakin tiliin liittämistäsi kirjautumistavoista." ]
        , Html.p [] [ text "Tarkastellaksesi kaikkia sovelluksella tallennettuja DMP:itä, klikkaa navigaatiopalkin otsikkoa 'DMP luettelo'. Luettelosta näet kaikkien DMP:iden otsikon, organisaation ja aineistojen määrän." ]
        , Html.p [] [ text "Tarkastellaksesi yhtä DMP:tä, klikkaa DMP:tä luettelosta. DMP:n sivulla näkyy kaikki kyseiselle DMP:lle tallennetut tiedot. Lisäksi DMP:n voi avata JSON-tiedostona 'Lataa JSON' painikkeesta ja sitten halutessaan ladata 'Save' painikkeesta. Mikäli tarkasteltava DMP kuuluu omalle organisaatiollesi, näet DMP:n sivulla myös 'Muokkaa' painikkeen. Sitä klikkaamalla avautuu lomakenäkymä, josta oman organisaation DMP:n tietoja voi muokata. Jos haluat poistaa oman organisaation DMP:n, löydät 'Poista DMP' painikkeen muokkauslomakkeen lopusta." ]
        , Html.p [] [ text "Luodaksesi uuden DMP:n organisaatiollesi, navigoi DMP luetteloon ja paina luettelon alla olevaa '+ Uusi DMP' painiketta. Tästä avautuu lomakenäkymä, johon täytetään DMP:n tiedot. DMP:n rakenne on hierarkinen ja se koostuu osioista, joista joitakin voi lisätä useamman kappaleen. Esimerkiksi, DMP:lle voi lisätä yhden tai useamman aineiston '+ Lisää aineisto' painikkeesta. Kun lisää aineiston, täytetään tiedot aineiston kenttiin, jonka lisäksi voidaan lisätä aineiston alle kuuluvia osioita, kuten jakelun tai metadatoja, aineiston alta löytyvistä '+ Lisää' painikkeista." ]
        , Html.p [] [ text "Lomakenäkymässä osion pakolliset kentät on merkitty * -merkillä. Huomaa, että myös vapaaehtoisten osioiden alla on * -merkittyjä kenttiä, jotka on pakko täyttää vain, jos lisäät kyseisen osion. Lomake tallennetaan lomakkeen alta löytyvästä 'Tallenna' painikkeesta. Jos saat virheen puuttuvasta kentästä, lue mitä kenttää virhe koskee ja palaa takaisin täyttämään kyseinen kenttä, ennen kuin yrität lähettää uudelleen. Tallennuksen jälkeen DMP tulee kaikille käyttäjille näkyviin DMP luetteloon, mutta on muokattavissa ja poistettavissa vain organisaatiosi jäsenille. Huomaa, että organisaatiolle voi halutessaan lisätä useita DMP:itä." ]
        , Html.p [] [ text "Mikäli haluat käyttää sovelluksen API:a (rajapintaa), klikkaa navigaatiopalkin otsikkoa 'API'. Näin pääset Swagger-sivulle, jossa voit suorittaa kyselyjä sovelluksen API:in. API:n endpointit palauttavat JSON-muotoista dataa. GET -endpointit eivät vaadi kirjautumista. Voit siis hakea joko kaikki DMP:t GET /dmp -endpointista tai hakea tiettyä DMP:tä sen ID:llä GET /dmp/{id} -endpointista. Muihin endpointteihin tarvitset Lajitietokeskus-tilisi personTokenin. Tämän kanssa voit lähettää DMP:itä POST /dmp -endpointtiin, poistaa oman organisaation DMP:itä DELETE /dmp/{id} -endpointilla ja muokata oman organisaation DMP:itä PUT /dmp/{id} -endpointilla. Näin ollen API on myös koneluettava ja -käytettävä (machine readable, machine actionable)." ]
        , Html.p [] [ text "Ongelmatilanteissa ota yhteyttä ", Html.a [ Html.Attributes.href "mailto:helpdesk@laji.fi" ] [ text "helpdesk@laji.fi" ]]
        , Html.p [] [ text "2025 Luomus" ]
        ]
  }
