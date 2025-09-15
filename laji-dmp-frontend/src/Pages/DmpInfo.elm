module Pages.DmpInfo exposing (..)

import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Html exposing (Html)
import Pages.DmpIndex exposing (Msg(..))
import Http
import Platform.Cmd as Cmd
import Html exposing (div)
import Array
import Html.Attributes exposing (class)
import Html exposing (h2, h3, h4, h5, p)
import User
import DmpApi exposing (getDmp)
import Models exposing (..)
import Utils exposing (..)
import Config exposing (Config)
import Html exposing (b)
import Html exposing (section)
import Array exposing (Array)
import Organization exposing (OrgLookup)

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

maybeFieldView : String -> Maybe String -> Html Msg
maybeFieldView label m = case m of
  Just str -> fieldView label str
  Nothing -> text ""

dmpIdView : DmpId -> Html Msg
dmpIdView dmpId = div []
  [ h4 [] [ text "DMP:n tunniste" ]
  , maybeFieldView "Tunniste: " dmpId.dmpIdIdentifier
  , fieldView "Tyyppi: " <| showDocumentIdType dmpId.dmpIdType
  ]

contactIdView : ContactId -> Html Msg
contactIdView contactId = div []
  [ h4 [] [ text "Kontaktin tunniste" ]
  , maybeFieldView "Tunniste: " contactId.contactIdIdentifier
  , fieldView "Tyyppi: " <| showPersonIdType contactId.contactIdType
  ]

contactView : Contact -> Html Msg
contactView contact = div []
  [ h3 [] [ text "Kontakti" ]
  , fieldView "Sähköpostiosoite: " contact.contactMbox
  , fieldView "Nimi: " contact.contactName
  , maybeFieldView "Organisaatio: " contact.contactOrganization
  , contactIdView contact.contactContactId
  ]

contributorIdView : ContributorId -> Html Msg
contributorIdView c = div []
  [ h4 [] [ text "Osallistujan tunniste" ]
  , maybeFieldView "Tunniste: " c.contributorIdIdentifier
  , fieldView "Tyyppi: " <| showPersonIdType c.contributorIdType
  ]

contributorView : Int -> Contributor -> Html Msg
contributorView idx c = div []
  [ h3 [] [ text <| "Osallistuja " ++ String.fromInt (idx + 1) ]
  , maybeFieldView "Sähköpostiosoite: " c.contributorMbox
  , fieldView "Nimi: " <| c.contributorName
  , maybeFieldView "Organisaatio: " c.contributorOrganization
  , fieldView "Rooli: " <| showRoleType c.contributorRole
  , contributorIdView c.contributorContributorId
  ]

contributorsView : Array Contributor -> Html Msg
contributorsView c = div [] <| Array.toList <| Array.indexedMap contributorView c

dataLifeCycleView : DataLifeCycle -> Html Msg
dataLifeCycleView d = div []
  [ h3 [] [ text <| "Datan elinkaari" ]
  , fieldView "Arkistointi: " <| showBool d.dataLifeCycleArchivingServicesData
  , fieldView "Datan varmuuskopiointi: " <| d.dataLifeCycleBackupData
  , fieldView "Datan poistaminen: " <| showDeletionDataType d.dataLifeCycleDeletionData
  , maybeFieldView "Datan poistamispäivä: " <| Maybe.map showDay d.dataLifeCycleDeletionWhenData
  ]

datasetIdView : DatasetId -> Html Msg
datasetIdView d = div []
  [ h4 [] [ text "Aineiston tunniste" ]
  , maybeFieldView "Tunniste: " d.datasetIdIdentifier
  , fieldView "Tyyppi: " <| showDocumentIdType d.datasetIdType
  ]

licenseView : Int -> License -> Html Msg
licenseView idx l = div []
  [ h5 [] [ text <| "Lisenssi " ++ String.fromInt (idx + 1) ]
  , fieldView "Käytetty lisenssi: " <| l.licenseRef
  , fieldView "Lisenssin käyttöönottopäivä: " <| showDay l.licenseStartDate
  ]

licensesView : Array License -> Html Msg
licensesView c = div [] <| Array.toList <| Array.indexedMap licenseView c

distributionView : Int -> Distribution -> Html Msg
distributionView idx d = div []
  [ h4 [] [ text <| "Julkaisu " ++ String.fromInt (idx + 1) ]
  , fieldView "Otsikko: " <| d.distributionTitle
  , maybeFieldView "Kuvaus: " d.distributionDescription
  , maybeFieldView "Julkaisun osoite: " d.distributionAccessUrl
  , maybeFieldView "Saatavuus: " <| Maybe.map showDataAccessType d.distributionDataAccess
  , maybeFieldView "Latausosoite: " d.distributionDownloadUri
  , maybeFieldView "Tiedostotyyppi: " d.distributionFormat
  , section [] [ licensesView d.distributionLicenses ]
  ]

distributionsView : Array Distribution -> Html Msg
distributionsView c = div [] <| Array.toList <| Array.indexedMap distributionView c

metadataIdView : MetadataId -> Html Msg
metadataIdView m = div []
  [ maybeFieldView "Tunniste: " m.metadataIdIdentifier
  , fieldView "Tyyppi: " <| showMetadataIdType m.metadataIdType
  ]

metadataView : Int -> Metadata -> Html Msg
metadataView idx m = div []
  [ h4 [] [ text <| "Metadata " ++ String.fromInt (idx + 1) ]
  , maybeFieldView "Dokumentaation avoimuus: " <| Maybe.map boolToString m.metadataAccessDocumentation
  , maybeFieldView "Datamalli: " m.metadataDataModel
  , maybeFieldView "Kuvaus: " m.metadataDescription
  , fieldView "Kieli: " <| showLanguage m.metadataLanguage
  , maybeFieldView "Dokumentaation sijainti: " m.metadataLocationDocumentation
  , maybeFieldView "Metadatan avoimuus: " <| Maybe.map boolToString m.metadataOpen
  , maybeFieldView "Metadatan osoite: " m.metadataLocation
  , maybeFieldView "Metadata perustuu tietomalliin: " <| Maybe.map boolToString m.metadataSchema
  , metadataIdView m.metadataMetadataId
  ]

metadatasView : Array Metadata -> Html Msg
metadatasView c = div [] <| Array.toList <| Array.indexedMap metadataView c

rightsView : Int -> RightsRelatedToData -> Html Msg
rightsView idx r = div []
  [ h4 [] [ text <| "Datan oikeudet " ++ String.fromInt (idx + 1) ]
  , maybeFieldView "Datan omistaja: " r.rightsOwnershipDataRight
  ]

rightsArrView : Array RightsRelatedToData -> Html Msg
rightsArrView c = div [] <| Array.toList <| Array.indexedMap rightsView c

securityView : Int -> SecurityAndPrivacy -> Html Msg
securityView idx s = div []
  [ h4 [] [ text <| "Tietoturva " ++ String.fromInt (idx + 1) ]
  , fieldView "Otsikko: " <| s.securityTitle
  , fieldView "Tietoturvakäytäntöjen kuvaus: " <| s.securityDescription
  ]

securityArrView : Array SecurityAndPrivacy -> Html Msg
securityArrView c = div [] <| Array.toList <| Array.indexedMap securityView c

datasetView : Int -> Dataset -> Html Msg
datasetView idx d = div []
  [ h3 [] [ text <| "Aineisto " ++ String.fromInt (idx + 1) ]
  , maybeFieldView "Laadunvarmistuksen kuvaus: " d.datasetDataQualityAssurance
  , maybeFieldView "Datanjakamisen haasteet: " d.datasetDataSharingIssues
  , maybeFieldView "Aineiston kuvaus: " d.datasetDescription
  , maybeFieldView "Aineiston tuotantoajankohta: " <| Maybe.map showDay d.datasetIssued
  , maybeFieldView "Avainsanat: " <| Maybe.map (Array.toList >> String.join ",") d.datasetKeywords
  , maybeFieldView "Kieli: " <| Maybe.map showLanguage d.datasetLanguage
  , fieldView "Henkilötiedot: " <| showPersonalDataType d.datasetPersonalData
  , fieldView "Sensitiivinen data: " <| showSensitiveDataType d.datasetSensitiveData
  , maybeFieldView "Aineston uudelleenkäyttö: " <| Maybe.map boolToString d.datasetReuseDataset
  , fieldView "Otsikko: " <| d.datasetTitle
  , maybeFieldView "Tyyppi: " d.datasetType
  , datasetIdView d.datasetDatasetId
  , section [] [ distributionsView d.datasetDistributions ]
  , section [] [ metadatasView d.datasetMetadata ]
  , section [] [ rightsArrView d.datasetRightsRelatedToData ]
  , section [] [ securityArrView d.datasetSecurityAndPrivacy ]
  ]

datasetsView : Array Dataset -> Html Msg
datasetsView c = div [] <| Array.toList <| Array.indexedMap datasetView c

ethicalIssueView : Int -> EthicalIssue -> Html Msg
ethicalIssueView idx s = div []
  [ h3 [] [ text <| "Eettiset haasteet " ++ String.fromInt (idx + 1) ]
  , maybeFieldView "Kuvaus eettisistä haasteista: " s.ethicalIssueDescription
  , fieldView "Eettisiä haasteita on: " <| showEthicalIssuesType s.ethicalIssueExist
  , maybeFieldView "Raportti eettisistä haasteista: " s.ethicalIssueReport
  ]

ethicalIssuesView : Array EthicalIssue -> Html Msg
ethicalIssuesView c = div [] <| Array.toList <| Array.indexedMap ethicalIssueView c

projectView : Int -> Project -> Html Msg
projectView idx s = div []
  [ h3 [] [ text <| "Projekti " ++ String.fromInt (idx + 1) ]
  , fieldView "Kuvaus: " <| s.projectDescription
  , maybeFieldView "Projektin loppumispäivä: " <| Maybe.map unwrapDay s.projectEndDate
  , fieldView "Projektin alkamispäivä: " <| unwrapDay s.projectStartDate
  , fieldView "Otsikko: " <| s.projectTitle
  ]

projectsView : Array Project -> Html Msg
projectsView c = div [] <| Array.toList <| Array.indexedMap projectView c

fieldView : String -> String -> Html Msg
fieldView label value = 
  div [ class "info-field" ] [ div [ class "field-label" ] [ text label ], div [ class "field-value" ] [ text value ] ]

dmpView : Dmp -> OrgLookup -> Html Msg
dmpView dmp orgs = div []
  [ fieldView "Otsikko: " dmp.dmpTitle
  , fieldView "Organisaatio: " <| showOrgName dmp orgs
  , maybeFieldView "Luomisaika: " <| Maybe.map showUtcTime dmp.dmpCreated
  , maybeFieldView "Muokkausaika: " <| Maybe.map showUtcTime dmp.dmpModified
  , maybeFieldView "Seuraava tarkastuspäivä: " <| Maybe.map showDay dmp.dmpNextReviewDmp
  , fieldView "Kieli: " <| showLanguage dmp.dmpLanguage
  , fieldView "Tyyppi: " <| showDmpType dmp.dmpTypeDmp
  , maybeFieldView "Kuvaus: " <| dmp.dmpDescription
  , dmpIdView dmp.dmpDmpId
  , contactView dmp.dmpContact
  , section [] [ contributorsView dmp.dmpContributors ]
  , Maybe.withDefault (text "") <| Maybe.map (\dlc -> section [] [ dataLifeCycleView dlc ]) dmp.dmpDataLifeCycle
  , section [] [ datasetsView dmp.dmpDatasets ]
  , section [] [ ethicalIssuesView dmp.dmpEthicalIssues ]
  , section [] [ projectsView dmp.dmpProjects ]
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
