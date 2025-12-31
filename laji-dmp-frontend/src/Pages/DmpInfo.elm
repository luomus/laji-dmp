module Pages.DmpInfo exposing (..)

import Array exposing (Array)
import Config exposing (Config)
import DmpApi exposing (getDmp)
import Http
import Html exposing (Html, a, div, h2, h3, h4, h5, section, table, tbody, td, text, tr)
import Html.Attributes exposing (class, href)
import Models exposing (..)
import Organization exposing (OrgLookup)
import Pages.DmpIndex exposing (Msg(..))
import Platform.Cmd as Cmd
import User
import Utils exposing (..)

type DmpState = Error String | Loading | HasDmp Dmp

type alias Model =
  { dmp: DmpState
  , session: User.LoginSession
  }

type Msg = GotDmpResponse (Result Http.Error Dmp)

init : Config -> String -> User.LoginSession -> ( Model, Cmd Msg )
init cfg idStr session =
  let maybeId = String.toInt idStr
  in case maybeId of
    Just id ->
      ({ dmp = Loading, session = session }, getDmp cfg id GotDmpResponse)
    Nothing -> ({ dmp = Error "No id provided", session = session }, Cmd.none)

update : User.LoginSession -> Msg -> Model -> (Model, Cmd Msg)
update session msg model =
  case msg of
    GotDmpResponse res ->
      case res of
        Ok dmp -> ({ model | dmp = HasDmp dmp, session = session }, Cmd.none)
        Err e ->
          ({ model | dmp = Error <| "Error loading DMP: " ++ httpErrorToString e, session = session }, Cmd.none)

fieldRow : String -> String -> Html Msg
fieldRow label value =
  tr [ class "info-field" ]
    [ td [ class "field-label" ] [ text label ]
    , td [ class "field-value" ] [ text value ]
    ]

maybeFieldRows : String -> Maybe String -> List (Html Msg)
maybeFieldRows label maybeValue =
  case maybeValue of
    Just value -> [ fieldRow label value ]
    Nothing -> []

fieldsTable : List (Html Msg) -> Html Msg
fieldsTable rows =
  table [ class "info-table" ] [ tbody [] rows ]

dmpIdRows : DmpId -> List (Html Msg)
dmpIdRows dmpId =
  maybeFieldRows "Vaihtoehtoinen tunniste: " dmpId.dmpIdIdentifier
    ++ [ fieldRow "Vaihtoehtoisen tunnisteen tyyppi: " <| showDocumentIdType dmpId.dmpIdType ]

contactIdRows : ContactId -> List (Html Msg)
contactIdRows contactId =
  maybeFieldRows "Tunniste: " contactId.contactIdIdentifier
    ++ [ fieldRow "Tunnisteen tyyppi: " <| showPersonIdType contactId.contactIdType ]

contactView : Contact -> Html Msg
contactView contact =
  div []
    [ h5 [] [ text <| "Kontakti" ]
    , fieldsTable <|
        [ fieldRow "Nimi: " contact.contactName
        , fieldRow "Sähköpostiosoite: " contact.contactMbox
        ]
          ++ (maybeFieldRows "Organisaatio: " contact.contactOrganization)
          ++ contactIdRows contact.contactContactId
    ]

contributorIdRows : ContributorId -> List (Html Msg)
contributorIdRows c =
  maybeFieldRows "Tunniste: " c.contributorIdIdentifier
    ++ [ fieldRow "Tunnisteen tyyppi: " <| showPersonIdType c.contributorIdType ]

contributorView : Int -> Contributor -> Html Msg
contributorView idx c =
  div [ class "dmp-info indent" ]
    [ h3 [] [ text <| "Osallistuja " ++ String.fromInt (idx + 1) ]
    , fieldsTable <|
        [ fieldRow "Nimi: " <| c.contributorName
        ]
          ++ (maybeFieldRows "Sähköpostiosoite: " c.contributorMbox)
          ++ (maybeFieldRows "Organisaatio: " c.contributorOrganization)
          ++ [ fieldRow "Rooli: " <| showRoleType c.contributorRole ]
          ++ contributorIdRows c.contributorContributorId
    ]

contributorsView : Array Contributor -> Html Msg
contributorsView c =
  div [] <| Array.toList <| Array.indexedMap contributorView c

projectView : Int -> Project -> Html Msg
projectView idx s =
  div [ class "dmp-info indent" ]
    [ h3 [] [ text <| "Projekti " ++ String.fromInt (idx + 1) ]
    , fieldsTable <|
        [ fieldRow "Nimi: " <| s.projectTitle
        , fieldRow "Kuvaus: " <| s.projectDescription
        , fieldRow "Projektin alkamispäivä: " <| unwrapDay s.projectStartDate
        ]
          ++ (maybeFieldRows "Projektin loppumispäivä: " <| Maybe.map unwrapDay s.projectEndDate)
    ]

projectsView : Array Project -> Html Msg
projectsView c =
  div [] <| Array.toList <| Array.indexedMap projectView c

dataLifeCycleView : DataLifeCycle -> Html Msg
dataLifeCycleView d =
  div [ class "dmp-info indent" ]
    [ h3 [] [ text <| "Datan elinkaari" ]
    , fieldsTable <|
        [ fieldRow "Arkistointi: " <| showBool d.dataLifeCycleArchivingServicesData
        , fieldRow "Datan varmuuskopiointi: " <| d.dataLifeCycleBackupData
        ]
          ++ (maybeFieldRows "Datan poistamispäivä: " <| Maybe.map showDay d.dataLifeCycleDeletionWhenData)
          ++ [ fieldRow "Päivityksen tiheys: " <| d.dataLifeCycleUpdateFrequency ]
    ]

datasetIdRows : DatasetId -> List (Html Msg)
datasetIdRows d =
  maybeFieldRows "Tunniste: " d.datasetIdIdentifier
    ++ [ fieldRow "Tunnisteen tyyppi: " <| showDocumentIdType d.datasetIdType ]

licenseView : Int -> License -> Html Msg
licenseView idx l =
  div [ class "dmp-info indent" ]
    [ h5 [] [ text <| "Lisenssi " ++ String.fromInt (idx + 1) ]
    , fieldsTable
        [ fieldRow "Käytettävä lisenssi: " <| l.licenseRef
        , fieldRow "Lisenssin käyttöönottopäivä: " <| showDay l.licenseStartDate
        ]
    ]

licensesView : Array License -> Html Msg
licensesView c =
  div [] <| Array.toList <| Array.indexedMap licenseView c

distributionView : Int -> Distribution -> Html Msg
distributionView idx d =
  div [ class "dmp-info indent" ]
    [ h4 [] [ text <| "Aineiston jakelu " ++ String.fromInt (idx + 1) ]
    , fieldsTable <|
        [ fieldRow "Otsikko: " <| d.distributionTitle
        ]
          ++ (maybeFieldRows "Jakelun osoite: " d.distributionAccessUrl)
          ++ (maybeFieldRows "Avoimuus: " <| Maybe.map showDataAccessType d.distributionDataAccess)
          ++ (maybeFieldRows "Lisätiedot: " d.distributionDescription)
          ++ (maybeFieldRows "Latausosoite: " d.distributionDownloadUri)
          ++ (maybeFieldRows "Tiedostotyyppi: " d.distributionFormat)
    , section [] [ licensesView d.distributionLicenses ]
    ]

distributionsView : Array Distribution -> Html Msg
distributionsView c =
  div [] <| Array.toList <| Array.indexedMap distributionView c

metadataIdRows : MetadataId -> List (Html Msg)
metadataIdRows m =
  maybeFieldRows "Tunniste: " m.metadataIdIdentifier
    ++ [ fieldRow "Tunnisteen tyyppi: " <| showMetadataIdType m.metadataIdType ]

metadataView : Int -> Metadata -> Html Msg
metadataView idx m =
  div [ class "dmp-info indent" ]
    [ h4 [] [ text <| "Metadata " ++ String.fromInt (idx + 1) ]
    , fieldsTable <|
        maybeFieldRows "Metadatan osoite: " m.metadataLocation
          ++ [ fieldRow "Kieli: " <| showLanguage m.metadataLanguage ]
          ++ (maybeFieldRows "Ovatko metatiedot avoimesti saatavilla?: " <| Maybe.map showBool m.metadataOpen)
          ++ (maybeFieldRows "Metadatan standardit: " <| Maybe.map (Array.toList >> String.join ",") m.metadataStandards)
          ++ metadataIdRows m.metadataMetadataId
    ]

metadatasView : Array Metadata -> Html Msg
metadatasView c =
  div [] <| Array.toList <| Array.indexedMap metadataView c

securityView : Int -> SecurityAndPrivacy -> Html Msg
securityView idx s =
  div [ class "dmp-info indent" ]
    [ h4 [] [ text <| "Tietoturva " ++ String.fromInt (idx + 1) ]
    , fieldsTable
        [ fieldRow "Tietoturvakäytännön nimi: " <| s.securityTitle
        , fieldRow "Tietoturvakäytäntöjen kuvaus: " <| s.securityDescription
        ]
    ]

securityArrView : Array SecurityAndPrivacy -> Html Msg
securityArrView c =
  div [] <| Array.toList <| Array.indexedMap securityView c

datasetView : Int -> Dataset -> Html Msg
datasetView idx d =
  div [ class "dmp-info indent" ]
    [ h3 [] [ text <| "Aineisto " ++ String.fromInt (idx + 1) ]
    , fieldsTable <|
        [ fieldRow "Nimi: " <| d.datasetTitle
        ]
          ++ (maybeFieldRows "Aineiston kuvaus: " d.datasetDescription)
          ++ [ fieldRow "Aineistosta vastaava taho: " <| d.datasetResponsiblePartyTitle
             , fieldRow "Vastaavan tahon sähköposti: " <| d.datasetResponsiblePartyEmail
             ]
          ++ (maybeFieldRows "Aineiston historiatiedot: " d.datasetLineage)
          ++ [ fieldRow "Tiedot saa viedä Luontotieto.fi -palveluun: " <| showBool d.datasetShareToSyke
             , fieldRow "Aineistotyyppi: " <| showDataType d.datasetDataType
             , fieldRow "Kieli: " <| showLanguage d.datasetLanguage
             ]
          ++ (maybeFieldRows "Avainsanat: " <| Maybe.map (Array.toList >> String.join ",") d.datasetKeywords)
          ++ (maybeFieldRows "Aineiston tuotantoajankohta: " <| Maybe.map showDay d.datasetIssued)
          ++ (maybeFieldRows "Onko aineisto tuotettu jo ennen projektia?: " <| Maybe.map showBool d.datasetReuseDataset)
          ++ [ fieldRow "Sisältääkö aineisto henkilötietoja?: " <| showPersonalDataType d.datasetPersonalData
             , fieldRow "Sisältääkö aineisto sensitiivistä dataa?: " <| showSensitiveDataType d.datasetSensitiveData
             ]
          ++ (maybeFieldRows "Laadunvarmistuksen kuvaus: " d.datasetDataQualityAssurance)
          ++ (maybeFieldRows "Datanjakamisen haasteet: " d.datasetDataSharingIssues)
          ++ (maybeFieldRows "Sanastot: " <| Maybe.map (Array.toList >> String.join ",") d.datasetVocabulary)
          ++ datasetIdRows d.datasetDatasetId
    , section [] [ distributionsView d.datasetDistributions ]
    , section [] [ metadatasView d.datasetMetadata ]
    , section [] [ securityArrView d.datasetSecurityAndPrivacy ]
    , Maybe.withDefault (text "") <| Maybe.map (\dlc -> section [] [ dataLifeCycleView dlc ]) d.datasetDataLifeCycle
    ]

datasetsView : Array Dataset -> Html Msg
datasetsView c =
  div [] <| Array.toList <| Array.indexedMap datasetView c

ethicalIssueView : Int -> EthicalIssue -> Html Msg
ethicalIssueView idx s =
  div [ class "dmp-info indent" ]
    [ h3 [] [ text <| "Eettiset haasteet " ++ String.fromInt (idx + 1) ]
    , fieldsTable <|
        [ fieldRow "Liittyykö dataan eettisiä haasteita?: " <| showEthicalIssuesType s.ethicalIssueExist
        ]
          ++ (maybeFieldRows "Kuvaus eettisistä haasteista: " s.ethicalIssueDescription)
          ++ (maybeFieldRows "Raportti eettisistä haasteista: " s.ethicalIssueReport)
    ]

ethicalIssuesView : Array EthicalIssue -> Html Msg
ethicalIssuesView c =
  div [] <| Array.toList <| Array.indexedMap ethicalIssueView c

dmpView : Dmp -> OrgLookup -> Html Msg
dmpView dmp orgs =
  let
    tunUrl id = "http://tun.fi/DMP." ++ String.fromInt id
  in
    div []
      [ fieldsTable <|
          [ fieldRow "Otsikko: " dmp.dmpTitle
          ]
            ++ (maybeFieldRows "Kuvaus: " <| dmp.dmpDescription)
            ++ [ fieldRow "Organisaatio: " <| showOrgName dmp orgs
               ]
            ++ (maybeFieldRows "Luomisaika: " <| Maybe.map showUtcTime dmp.dmpCreated)
            ++ (maybeFieldRows "Muokkausaika: " <| Maybe.map showUtcTime dmp.dmpModified)
            ++ (maybeFieldRows "Muokkausaika: " <| Maybe.map showUtcTime dmp.dmpModified)
            ++ (case dmp.dmpId of 
              Just id -> [ tr [ class "info-field" ]
                [ td [ class "field-label" ] [ text "Id: " ]
                , td [ class "field-value" ] [ a [ href <| tunUrl id ] [ text <| tunUrl id ] ]
                ] ]
              Nothing -> []
            ) ++ (maybeFieldRows "Seuraava tarkastuspäivä: " <| Maybe.map showDay dmp.dmpNextReviewDmp)
            ++ dmpIdRows dmp.dmpDmpId
      , contactView dmp.dmpContact
      , section [] [ contributorsView dmp.dmpContributors ]
      , section [] [ projectsView dmp.dmpProjects ]
      , section [] [ datasetsView dmp.dmpDatasets ]
      , section [] [ ethicalIssuesView dmp.dmpEthicalIssues ]
      ]

view : Config -> Model -> OrgLookup -> { title : String, body : Html Msg }
view cfg model orgs =
  { title = "DMP:n tiedot"
  , body =
    div [ class "dmp-info" ] <| case model.dmp of
      HasDmp dmp ->
        case dmp.dmpId of
          Just id ->
            [ h2 [] [ text <| "DMP " ++ (String.fromInt id) ++ " tiedot" ]
            , div [] <| case model.session of
              User.LoggedIn _ person ->
                if Array.length (Array.filter (\r -> r == User.Admin) person.role) > 0
                  then [ a [href <| "/dmp/" ++ (String.fromInt id) ++"/edit", class "btn"] [text "Muokkaa"] ]
                  else []
              _ -> []
            , div [] [a [class "btn", href <| cfg.dmpApiBase ++ "/dmp/" ++ String.fromInt id] [text "Lataa JSON ↗︎"]]
            , dmpView dmp orgs
            ]
          Nothing -> [text "Virhe: DMP:n tunniste puuttuu"]
      Loading -> [text "Ladataan DMP:tä..."]
      Error e -> [text <| "Virhe: " ++ e]
  }
