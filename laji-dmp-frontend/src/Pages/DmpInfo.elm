module Pages.DmpInfo exposing (..)

import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
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

type DmpState = Error | Loading | HasDmp Dmp

type alias Model =
  { dmp: DmpState
  , session: User.LoginSession
  }

type Msg = GotDmpResponse (Result Http.Error Dmp)

init : String -> User.LoginSession -> ( Model, Cmd Msg )
init idStr session =
  let maybeId = String.toInt idStr
  in case maybeId of
    Just id ->
      ({ dmp = Loading, session = session }, getDmp id GotDmpResponse)
    Nothing -> ({ dmp = Error, session = session }, Cmd.none)

update : User.LoginSession -> Msg -> Model -> (Model, Cmd Msg)
update session msg model =
  case msg of
    GotDmpResponse res ->
      case res of
        Ok dmp -> ({ model | dmp = HasDmp dmp, session = session }, Cmd.none)
        Err e ->
          let _ = Debug.log "Error loading DMP" e
          in ({ model | dmp = Error, session = session }, Cmd.none)

dmpIdView : DmpId -> Html Msg
dmpIdView dmpId = div []
  [ h3 [] [ text "Dmp Id" ]
  , p [] [ text <| "Identifier: " ++ Debug.toString dmpId.dmpIdIdentifier ]
  , p [] [ text <| "Type: " ++ showDocumentIdType dmpId.dmpIdType ]
  ]

contactIdView : ContactId -> Html Msg
contactIdView contactId = div []
  [ h3 [] [ text "Contact Id" ]
  , p [] [ text <| "Identifier: " ++ Debug.toString contactId.contactIdIdentifier ]
  , p [] [ text <| "Type: " ++ showPersonIdType contactId.contactIdType ]
  ]

contactView : Contact -> Html Msg
contactView contact = div []
  [ h3 [] [ text "Contact" ]
  , p [] [ text <| "Mbox: " ++ contact.contactMbox ]
  , p [] [ text <| "Name: " ++ contact.contactName ]
  , p [] [ text <| "Organization: " ++ Debug.toString contact.contactOrganization ]
  , contactIdView contact.contactContactId
  ]

contributorIdView : ContributorId -> Html Msg
contributorIdView c = div []
  [ h4 [] [ text "Contributor id" ]
  , p [] [ text <| "Identifier: " ++ Debug.toString c.contributorIdIdentifier ]
  , p [] [ text <| "Type: " ++ showPersonIdType c.contributorIdType ]
  ]

contributorView : Contributor -> Html Msg
contributorView c = div []
  [ h3 [] [ text "Contributor" ]
  , p [] [ text <| "Mbox: " ++ Debug.toString c.contributorMbox ]
  , p [] [ text <| "Name: " ++ c.contributorName ]
  , p [] [ text <| "Organization: " ++ Debug.toString c.contributorOrganization ]
  , p [] [ text <| "Role: " ++ showRoleType c.contributorRole ]
  , contributorIdView c.contributorContributorId
  ]

contributorsView : Array.Array Contributor -> Html Msg
contributorsView c = div [] <| Array.toList <| Array.map contributorView c

dataLifeCycleView : DataLifeCycle -> Html Msg
dataLifeCycleView d = div []
  [ h3 [] [ text "Data life cycle" ]
  , p [] [ text <| "Archiving services data: " ++ Debug.toString d.dataLifeCycleArchivingServicesData ]
  , p [] [ text <| "Backup data: " ++ d.dataLifeCycleBackupData ]
  , p [] [ text <| "Deletion data type: " ++ showDeletionDataType d.dataLifeCycleDeletionData ]
  , p [] [ text <| "Deletion when data: " ++ (Debug.toString <| Maybe.map showDay d.dataLifeCycleDeletionWhenData) ]
  ]

dataLifeCyclesView : Array.Array DataLifeCycle -> Html Msg
dataLifeCyclesView c = div [] <| Array.toList <| Array.map dataLifeCycleView c

datasetIdView : DatasetId -> Html Msg
datasetIdView d = div []
  [ h4 [] [ text "Dataset id" ]
  , p [] [ text <| "Identifier: " ++ (Debug.toString d.datasetIdIdentifier) ]
  , p [] [ text <| "Type: " ++ showDocumentIdType d.datasetIdType ]
  ]

licenseView : License -> Html Msg
licenseView l = div []
  [ h5 [] [ text "License" ]
  , p [] [ text <| "Ref: " ++ l.licenseRef ]
  , p [] [ text <| "Start date: " ++ showDay l.licenseStartDate ]
  ]

licensesView : Array.Array License -> Html Msg
licensesView c = div [] <| Array.toList <| Array.map licenseView c

distributionView : Distribution -> Html Msg
distributionView d = div []
  [ h4 [] [ text "Distribution" ]
  , p [] [ text <| "Title: " ++ d.distributionTitle ]
  , p [] [ text <| "Description: " ++ (Debug.toString d.distributionDescription) ]
  , p [] [ text <| "Access URL: " ++ (Debug.toString d.distributionAccessUrl) ]
  , p [] [ text <| "Data access: " ++ (Debug.toString <| Maybe.map showDataAccessType d.distributionDataAccess) ]
  , p [] [ text <| "Download URI: " ++ (Debug.toString d.distributionDownloadUri) ]
  , p [] [ text <| "Format: " ++ (Debug.toString d.distributionFormat) ]
  , div [] [ licensesView d.distributionLicenses ]
  ]

distributionsView : Array.Array Distribution -> Html Msg
distributionsView c = div [] <| Array.toList <| Array.map distributionView c

metadataIdView : MetadataId -> Html Msg
metadataIdView m = div []
  [ p [] [ text <| "Identifier: " ++ Debug.toString m.metadataIdIdentifier ]
  , p [] [ text <| "Type: " ++ showMetadataIdType m.metadataIdType ]
  ]

metadataView : Metadata -> Html Msg
metadataView m = div []
  [ h4 [] [ text "Metadata" ]
  , p [] [ text <| "Access documentation: " ++ ( Debug.toString <| Maybe.map Debug.toString m.metadataAccessDocumentation ) ]
  , p [] [ text <| "Data model: " ++ ( Debug.toString <| m.metadataDataModel ) ]
  , p [] [ text <| "Description: " ++ ( Debug.toString <| m.metadataDescription) ]
  , p [] [ text <| "Language: " ++ showLanguage m.metadataLanguage ]
  , p [] [ text <| "Location documentation: " ++ ( Debug.toString <| m.metadataLocationDocumentation) ]
  , p [] [ text <| "Open: " ++ ( Debug.toString <| Maybe.map Debug.toString m.metadataOpen) ]
  , p [] [ text <| "Location: " ++ ( Debug.toString <| m.metadataLocation) ]
  , p [] [ text <| "Schema: " ++ ( Debug.toString <| Maybe.map Debug.toString m.metadataSchema) ]
  , metadataIdView m.metadataMetadataId
  ]

metadatasView : Array.Array Metadata -> Html Msg
metadatasView c = div [] <| Array.toList <| Array.map metadataView c

rightsView : RightsRelatedToData -> Html Msg
rightsView r = div []
  [ h4 [] [ text "Rights related to data" ]
  , p [] [ text <| "Ownership data right: " ++ Debug.toString r.rightsOwnershipDataRight ]
  ]

rightsArrView : Array.Array RightsRelatedToData -> Html Msg
rightsArrView c = div [] <| Array.toList <| Array.map rightsView c

securityView : SecurityAndPrivacy -> Html Msg
securityView s = div []
  [ h4 [] [ text "Security and privacy" ]
  , p [] [ text <| "Title: " ++ s.securityTitle ]
  , p [] [ text <| "Description: " ++ s.securityDescription ]
  ]

securityArrView : Array.Array SecurityAndPrivacy -> Html Msg
securityArrView c = div [] <| Array.toList <| Array.map securityView c

datasetView : Dataset -> Html Msg
datasetView d = div []
  [ h3 [] [ text <| "Dataset" ]
  , p [] [ text <| "Data quality assurance: " ++ Debug.toString d.datasetDataQualityAssurance ]
  , p [] [ text <| "Data sharing issues: " ++ Debug.toString d.datasetDataSharingIssues ]
  , p [] [ text <| "Description: " ++ Debug.toString d.datasetDescription ]
  , p [] [ text <| "Issued: " ++ (Debug.toString <| Maybe.map showDay d.datasetIssued) ]
  , p [] [ text <| "Keywords: " ++ (Debug.toString <| Maybe.map (Array.toList >> String.join ",") d.datasetKeywords) ]
  , p [] [ text <| "Language: " ++ (Debug.toString <| Maybe.map showLanguage d.datasetLanguage) ]
  , p [] [ text <| "Personal data: " ++ showPersonalDataType d.datasetPersonalData ]
  , p [] [ text <| "Sensitive data: " ++ showSensitiveDataType d.datasetSensitiveData ]
  , p [] [ text <| "Reuse dataset: " ++ (Debug.toString <| Maybe.map Debug.toString d.datasetReuseDataset) ]
  , p [] [ text <| "Title: " ++ d.datasetTitle ]
  , p [] [ text <| "Type: " ++ (Debug.toString d.datasetType) ]
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
  [ h3 [] [ text "Ethical issue" ]
  , p [] [ text <| "Description: " ++ Debug.toString s.ethicalIssueDescription ]
  , p [] [ text <| "Exist: " ++ showEthicalIssuesType s.ethicalIssueExist ]
  , p [] [ text <| "Report: " ++ Debug.toString s.ethicalIssueReport ]
  ]

ethicalIssuesView : Array.Array EthicalIssue -> Html Msg
ethicalIssuesView c = div [] <| Array.toList <| Array.map ethicalIssueView c

projectView : Project -> Html Msg
projectView s = div []
  [ h3 [] [ text "Project" ]
  , p [] [ text <| "Description: " ++ s.projectDescription ]
  , p [] [ text <| "End date: " ++ Debug.toString (Maybe.map unwrapDay s.projectEndDate) ]
  , p [] [ text <| "Start date: " ++ unwrapDay s.projectStartDate ]
  , p [] [ text <| "Title: " ++ s.projectTitle ]
  ]

projectsView : Array.Array Project -> Html Msg
projectsView c = div [] <| Array.toList <| Array.map projectView c

dmpView : Dmp -> Html Msg
dmpView dmp = div []
  [ p [] [ text <| "Title: " ++ dmp.dmpTitle ]
  , p [] [ text <| "Organization: " ++ dmp.dmpOrgId ]
  , p [] [ text <| "Created: " ++ Debug.toString (Maybe.map showUtcTime dmp.dmpCreated) ]
  , p [] [ text <| "Modified: " ++ Debug.toString (Maybe.map showUtcTime dmp.dmpModified) ]
  , p [] [ text <| "Next review: " ++ Debug.toString (Maybe.map showDay dmp.dmpNextReviewDmp) ]
  , p [] [ text <| "Language: " ++ showLanguage dmp.dmpLanguage ]
  , p [] [ text <| "Type: " ++ showDmpType dmp.dmpTypeDmp ]
  , p [] [ text <| "Description: " ++ Debug.toString dmp.dmpDescription ]
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
  { title = "Dmp Info View"
  , body =
    div [] <| case model.dmp of
      HasDmp dmp ->
        case dmp.dmpId of
          Just id ->
            [ h2 [] [ text <| "DMP " ++ (String.fromInt id) ++ " info" ]
            , dmpView dmp
            , div [] <| case model.session of
              User.LoggedIn _ person ->
                if Array.length (Array.filter (\r -> r == User.Admin) person.role) > 0
                  then [ a [href <| "/dmp/" ++ (String.fromInt id) ++"/edit", class "btn"] [text "Edit"] ]
                  else []
              _ -> []
            -- , div [] <| Array.toList <| Array.indexedMap datasetView dmp.datasets
            ]
          Nothing -> [text "Error: expected DMP to have an id"]
      Loading -> [text "Loading data management plan..."]
      Error -> [text "Error loading data management plan"]
  }
