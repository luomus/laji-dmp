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
  Just str -> p [] [ text <| label ++ str ]
  Nothing -> text ""

dmpIdView : DmpId -> Html Msg
dmpIdView dmpId = div []
  [ h3 [] [ text "DMP:n tunniste" ]
  , maybeFieldView "Tunniste: " dmpId.dmpIdIdentifier
  , p [] [ text <| "Tyyppi: " ++ showDocumentIdType dmpId.dmpIdType ]
  ]

contactIdView : ContactId -> Html Msg
contactIdView contactId = div []
  [ h3 [] [ text "Kontaktin tunniste" ]
  , maybeFieldView "Tunniste: " contactId.contactIdIdentifier
  , p [] [ text <| "Tyyppi: " ++ showPersonIdType contactId.contactIdType ]
  ]

contactView : Contact -> Html Msg
contactView contact = div []
  [ h3 [] [ text "Kontakti" ]
  , p [] [ text <| "Sähköpostiosoite: " ++ contact.contactMbox ]
  , p [] [ text <| "Nimi: " ++ contact.contactName ]
  , maybeFieldView "Organisaatio: " contact.contactOrganization
  , contactIdView contact.contactContactId
  ]

contributorIdView : ContributorId -> Html Msg
contributorIdView c = div []
  [ h4 [] [ text "Osallistujan tunniste" ]
  , maybeFieldView "Tunniste: " c.contributorIdIdentifier
  , p [] [ text <| "Tyyppi: " ++ showPersonIdType c.contributorIdType ]
  ]

contributorView : Contributor -> Html Msg
contributorView c = div []
  [ h3 [] [ text "Osallistuja" ]
  , maybeFieldView "Sähköpostiosoite: " c.contributorMbox
  , p [] [ text <| "Nimi: " ++ c.contributorName ]
  , maybeFieldView "Organisaatio: " c.contributorOrganization
  , p [] [ text <| "Rooli: " ++ showRoleType c.contributorRole ]
  , contributorIdView c.contributorContributorId
  ]

contributorsView : Array.Array Contributor -> Html Msg
contributorsView c = div [] <| Array.toList <| Array.map contributorView c

dataLifeCycleView : DataLifeCycle -> Html Msg
dataLifeCycleView d = div []
  [ h3 [] [ text "Datan elinkaari" ]
  , p [] [ text <| "Arkistointi: " ++ boolToString d.dataLifeCycleArchivingServicesData ]
  , p [] [ text <| "Datan varmuuskopiointi: " ++ d.dataLifeCycleBackupData ]
  , p [] [ text <| "Datan poistaminen: " ++ showDeletionDataType d.dataLifeCycleDeletionData ]
  , maybeFieldView "Datan poistamispäivä: " <| Maybe.map showDay d.dataLifeCycleDeletionWhenData
  ]

dataLifeCyclesView : Array.Array DataLifeCycle -> Html Msg
dataLifeCyclesView c = div [] <| Array.toList <| Array.map dataLifeCycleView c

datasetIdView : DatasetId -> Html Msg
datasetIdView d = div []
  [ h4 [] [ text "Aineiston tunniste" ]
  , maybeFieldView "Tunniste: " d.datasetIdIdentifier
  , p [] [ text <| "Tyyppi: " ++ showDocumentIdType d.datasetIdType ]
  ]

licenseView : License -> Html Msg
licenseView l = div []
  [ h5 [] [ text "Lisenssi" ]
  , p [] [ text <| "Käytetty lisenssi: " ++ l.licenseRef ]
  , p [] [ text <| "Lisenssin käyttöönottopäivä: " ++ showDay l.licenseStartDate ]
  ]

licensesView : Array.Array License -> Html Msg
licensesView c = div [] <| Array.toList <| Array.map licenseView c

distributionView : Distribution -> Html Msg
distributionView d = div []
  [ h4 [] [ text "Julkaisu" ]
  , p [] [ text <| "Otsikko: " ++ d.distributionTitle ]
  , maybeFieldView "Kuvaus: " d.distributionDescription
  , maybeFieldView "Julkaisun osoite: " d.distributionAccessUrl
  , maybeFieldView "Saatavuus: " <| Maybe.map showDataAccessType d.distributionDataAccess
  , maybeFieldView "Latausosoite: " d.distributionDownloadUri
  , maybeFieldView "Tiedostotyyppi: " d.distributionFormat
  , div [] [ licensesView d.distributionLicenses ]
  ]

distributionsView : Array.Array Distribution -> Html Msg
distributionsView c = div [] <| Array.toList <| Array.map distributionView c

metadataIdView : MetadataId -> Html Msg
metadataIdView m = div []
  [ maybeFieldView "Tunniste: " m.metadataIdIdentifier
  , p [] [ text <| "Tyyppi: " ++ showMetadataIdType m.metadataIdType ]
  ]

metadataView : Metadata -> Html Msg
metadataView m = div []
  [ h4 [] [ text "Metadata" ]
  , maybeFieldView "Dokumentaation avoimuus: " <| Maybe.map boolToString m.metadataAccessDocumentation
  , maybeFieldView "Datamalli: " m.metadataDataModel
  , maybeFieldView "Kuvaus: " m.metadataDescription
  , p [] [ text <| "Kieli: " ++ showLanguage m.metadataLanguage ]
  , maybeFieldView "Dokumentaation sijainti: " m.metadataLocationDocumentation
  , maybeFieldView "Metadatan avoimuus: " <| Maybe.map boolToString m.metadataOpen
  , maybeFieldView "Metadatan osoite: " m.metadataLocation
  , maybeFieldView "Metadata perustuu tietomalliin: " <| Maybe.map boolToString m.metadataSchema
  , metadataIdView m.metadataMetadataId
  ]

metadatasView : Array.Array Metadata -> Html Msg
metadatasView c = div [] <| Array.toList <| Array.map metadataView c

rightsView : RightsRelatedToData -> Html Msg
rightsView r = div []
  [ h4 [] [ text "Datan oikeudet" ]
  , maybeFieldView "Datan omistaja: " r.rightsOwnershipDataRight
  ]

rightsArrView : Array.Array RightsRelatedToData -> Html Msg
rightsArrView c = div [] <| Array.toList <| Array.map rightsView c

securityView : SecurityAndPrivacy -> Html Msg
securityView s = div []
  [ h4 [] [ text "Tietoturva" ]
  , p [] [ text <| "Otsikko: " ++ s.securityTitle ]
  , p [] [ text <| "Tietoturvakäytäntöjen kuvaus: " ++ s.securityDescription ]
  ]

securityArrView : Array.Array SecurityAndPrivacy -> Html Msg
securityArrView c = div [] <| Array.toList <| Array.map securityView c

datasetView : Dataset -> Html Msg
datasetView d = div []
  [ h3 [] [ text <| "Aineisto" ]
  , maybeFieldView "Laadunvarmistuksen kuvaus: " d.datasetDataQualityAssurance
  , maybeFieldView "Datanjakamisen haasteet: " d.datasetDataSharingIssues
  , maybeFieldView "Aineiston kuvaus: " d.datasetDescription
  , maybeFieldView "Aineiston tuotantoajankohta: " <| Maybe.map showDay d.datasetIssued
  , maybeFieldView "Avainsanat: " <| Maybe.map (Array.toList >> String.join ",") d.datasetKeywords
  , maybeFieldView "Kieli: " <| Maybe.map showLanguage d.datasetLanguage
  , p [] [ text <| "Henkilötiedot: " ++ showPersonalDataType d.datasetPersonalData ]
  , p [] [ text <| "Sensitiivinen data: " ++ showSensitiveDataType d.datasetSensitiveData ]
  , maybeFieldView "Aineston uudelleenkäyttö: " <| Maybe.map boolToString d.datasetReuseDataset
  , p [] [ text <| "Otsikko: " ++ d.datasetTitle ]
  , maybeFieldView "Tyyppi: " d.datasetType
  , datasetIdView d.datasetDatasetId
  , distributionsView d.datasetDistributions
  , metadatasView d.datasetMetadata
  , rightsArrView d.datasetRightsRelatedToData
  , securityArrView d.datasetSecurityAndPrivacy
  ]

datasetsView : Array.Array Dataset -> Html Msg
datasetsView c = div [] <| Array.toList <| Array.map datasetView c

ethicalIssueView : EthicalIssue -> Html Msg
ethicalIssueView s = div []
  [ h3 [] [ text "Eettiset haasteet" ]
  , maybeFieldView "Kuvaus eettisistä haasteista: " s.ethicalIssueDescription
  , p [] [ text <| "Eettisiä haasteita on: " ++ showEthicalIssuesType s.ethicalIssueExist ]
  , maybeFieldView "Raportti eettisistä haasteista: " s.ethicalIssueReport
  ]

ethicalIssuesView : Array.Array EthicalIssue -> Html Msg
ethicalIssuesView c = div [] <| Array.toList <| Array.map ethicalIssueView c

projectView : Project -> Html Msg
projectView s = div []
  [ h3 [] [ text "Projekti" ]
  , p [] [ text <| "Kuvaus: " ++ s.projectDescription ]
  , maybeFieldView "Projektin loppumispäivä: " <| Maybe.map unwrapDay s.projectEndDate
  , p [] [ text <| "Projektin alkamispäivä: " ++ unwrapDay s.projectStartDate ]
  , p [] [ text <| "Otsikko: " ++ s.projectTitle ]
  ]

projectsView : Array.Array Project -> Html Msg
projectsView c = div [] <| Array.toList <| Array.map projectView c

dmpView : Dmp -> Html Msg
dmpView dmp = div []
  [ p [] [ text <| "Otsikko: " ++ dmp.dmpTitle ]
  , p [] [ text <| "Organisaatio: " ++ dmp.dmpOrgId ]
  , maybeFieldView "Luomisaika: " <| Maybe.map showUtcTime dmp.dmpCreated
  , maybeFieldView "Muokkausaika: " <| Maybe.map showUtcTime dmp.dmpModified
  , maybeFieldView "Seuraava tarkastuspäivä: " <| Maybe.map showDay dmp.dmpNextReviewDmp
  , p [] [ text <| "Kieli: " ++ showLanguage dmp.dmpLanguage ]
  , p [] [ text <| "Tyyppi: " ++ showDmpType dmp.dmpTypeDmp ]
  , maybeFieldView "Kuvaus: " <| dmp.dmpDescription
  , dmpIdView dmp.dmpDmpId
  , contactView dmp.dmpContact
  , contributorsView dmp.dmpContributors
  , dataLifeCyclesView dmp.dmpDataLifeCycles
  , datasetsView dmp.dmpDatasets
  , ethicalIssuesView dmp.dmpEthicalIssues
  , projectsView dmp.dmpProjects
  ]

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "DMP:n tiedot"
  , body =
    div [] <| case model.dmp of
      HasDmp dmp ->
        case dmp.dmpId of
          Just id ->
            [ h2 [] [ text <| "DMP " ++ (String.fromInt id) ++ " tiedot" ]
            , dmpView dmp
            , div [] <| case model.session of
              User.LoggedIn _ person ->
                if Array.length (Array.filter (\r -> r == User.Admin) person.role) > 0
                  then [ a [href <| "/dmp/" ++ (String.fromInt id) ++"/edit", class "btn"] [text "Muokkaa"] ]
                  else []
              _ -> []
            ]
          Nothing -> [text "Virhe: DMP:n tunniste puuttuu"]
      Loading -> [text "Ladataan DMP:tä..."]
      Error e -> [text <| "Virhe: " ++ e]
  }
