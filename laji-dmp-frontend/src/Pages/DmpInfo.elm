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
  [ h3 [] [ text "Dmp Id" ]
  , maybeFieldView "Identifier: " dmpId.dmpIdIdentifier
  , p [] [ text <| "Type: " ++ showDocumentIdType dmpId.dmpIdType ]
  ]

contactIdView : ContactId -> Html Msg
contactIdView contactId = div []
  [ h3 [] [ text "Contact Id" ]
  , maybeFieldView "Identifier: " contactId.contactIdIdentifier
  , p [] [ text <| "Type: " ++ showPersonIdType contactId.contactIdType ]
  ]

contactView : Contact -> Html Msg
contactView contact = div []
  [ h3 [] [ text "Contact" ]
  , p [] [ text <| "Mbox: " ++ contact.contactMbox ]
  , p [] [ text <| "Name: " ++ contact.contactName ]
  , maybeFieldView "Organization: " contact.contactOrganization
  , contactIdView contact.contactContactId
  ]

contributorIdView : ContributorId -> Html Msg
contributorIdView c = div []
  [ h4 [] [ text "Contributor id" ]
  , maybeFieldView "Identifier: " c.contributorIdIdentifier
  , p [] [ text <| "Type: " ++ showPersonIdType c.contributorIdType ]
  ]

contributorView : Contributor -> Html Msg
contributorView c = div []
  [ h3 [] [ text "Contributor" ]
  , maybeFieldView "Mbox: " c.contributorMbox
  , p [] [ text <| "Name: " ++ c.contributorName ]
  , maybeFieldView "Organization: " c.contributorOrganization
  , p [] [ text <| "Role: " ++ showRoleType c.contributorRole ]
  , contributorIdView c.contributorContributorId
  ]

contributorsView : Array.Array Contributor -> Html Msg
contributorsView c = div [] <| Array.toList <| Array.map contributorView c

dataLifeCycleView : DataLifeCycle -> Html Msg
dataLifeCycleView d = div []
  [ h3 [] [ text "Data life cycle" ]
  , p [] [ text <| "Archiving services data: " ++ boolToString d.dataLifeCycleArchivingServicesData ]
  , p [] [ text <| "Backup data: " ++ d.dataLifeCycleBackupData ]
  , p [] [ text <| "Deletion data type: " ++ showDeletionDataType d.dataLifeCycleDeletionData ]
  , maybeFieldView "Deletion when data: " <| Maybe.map showDay d.dataLifeCycleDeletionWhenData
  ]

dataLifeCyclesView : Array.Array DataLifeCycle -> Html Msg
dataLifeCyclesView c = div [] <| Array.toList <| Array.map dataLifeCycleView c

datasetIdView : DatasetId -> Html Msg
datasetIdView d = div []
  [ h4 [] [ text "Dataset id" ]
  , maybeFieldView "Identifier: " d.datasetIdIdentifier
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
  , maybeFieldView "Description: " d.distributionDescription
  , maybeFieldView "Access URL: " d.distributionAccessUrl
  , maybeFieldView "Data access: " <| Maybe.map showDataAccessType d.distributionDataAccess
  , maybeFieldView "Download URI: " d.distributionDownloadUri
  , maybeFieldView "Format: " d.distributionFormat
  , div [] [ licensesView d.distributionLicenses ]
  ]

distributionsView : Array.Array Distribution -> Html Msg
distributionsView c = div [] <| Array.toList <| Array.map distributionView c

metadataIdView : MetadataId -> Html Msg
metadataIdView m = div []
  [ maybeFieldView "Identifier: " m.metadataIdIdentifier
  , p [] [ text <| "Type: " ++ showMetadataIdType m.metadataIdType ]
  ]

metadataView : Metadata -> Html Msg
metadataView m = div []
  [ h4 [] [ text "Metadata" ]
  , maybeFieldView "Access documentation: " <| Maybe.map boolToString m.metadataAccessDocumentation
  , maybeFieldView "Data model: " m.metadataDataModel
  , maybeFieldView "Description: " m.metadataDescription
  , p [] [ text <| "Language: " ++ showLanguage m.metadataLanguage ]
  , maybeFieldView "Location documentation: " m.metadataLocationDocumentation
  , maybeFieldView "Open: " <| Maybe.map boolToString m.metadataOpen
  , maybeFieldView "Location: " m.metadataLocation
  , maybeFieldView "Schema: " <| Maybe.map boolToString m.metadataSchema
  , metadataIdView m.metadataMetadataId
  ]

metadatasView : Array.Array Metadata -> Html Msg
metadatasView c = div [] <| Array.toList <| Array.map metadataView c

rightsView : RightsRelatedToData -> Html Msg
rightsView r = div []
  [ h4 [] [ text "Rights related to data" ]
  , maybeFieldView "Ownership data right: " r.rightsOwnershipDataRight
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
  , maybeFieldView "Data quality assurance: " d.datasetDataQualityAssurance
  , maybeFieldView "Data sharing issues: " d.datasetDataSharingIssues
  , maybeFieldView "Description: " d.datasetDescription
  , maybeFieldView "Issued: " <| Maybe.map showDay d.datasetIssued
  , maybeFieldView "Keywords: " <| Maybe.map (Array.toList >> String.join ",") d.datasetKeywords
  , maybeFieldView "Language: " <| Maybe.map showLanguage d.datasetLanguage
  , p [] [ text <| "Personal data: " ++ showPersonalDataType d.datasetPersonalData ]
  , p [] [ text <| "Sensitive data: " ++ showSensitiveDataType d.datasetSensitiveData ]
  , maybeFieldView "Reuse dataset: " <| Maybe.map boolToString d.datasetReuseDataset
  , p [] [ text <| "Title: " ++ d.datasetTitle ]
  , maybeFieldView "Type: " d.datasetType
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
  , maybeFieldView "Description: " s.ethicalIssueDescription
  , p [] [ text <| "Exist: " ++ showEthicalIssuesType s.ethicalIssueExist ]
  , maybeFieldView "Report: " s.ethicalIssueReport
  ]

ethicalIssuesView : Array.Array EthicalIssue -> Html Msg
ethicalIssuesView c = div [] <| Array.toList <| Array.map ethicalIssueView c

projectView : Project -> Html Msg
projectView s = div []
  [ h3 [] [ text "Project" ]
  , p [] [ text <| "Description: " ++ s.projectDescription ]
  , maybeFieldView "End date: " <| Maybe.map unwrapDay s.projectEndDate
  , p [] [ text <| "Start date: " ++ unwrapDay s.projectStartDate ]
  , p [] [ text <| "Title: " ++ s.projectTitle ]
  ]

projectsView : Array.Array Project -> Html Msg
projectsView c = div [] <| Array.toList <| Array.map projectView c

dmpView : Dmp -> Html Msg
dmpView dmp = div []
  [ p [] [ text <| "Title: " ++ dmp.dmpTitle ]
  , p [] [ text <| "Organization: " ++ dmp.dmpOrgId ]
  , maybeFieldView "Created: " <| Maybe.map showUtcTime dmp.dmpCreated
  , maybeFieldView "Modified: " <| Maybe.map showUtcTime dmp.dmpModified
  , maybeFieldView "Next review: " <| Maybe.map showDay dmp.dmpNextReviewDmp
  , p [] [ text <| "Language: " ++ showLanguage dmp.dmpLanguage ]
  , p [] [ text <| "Type: " ++ showDmpType dmp.dmpTypeDmp ]
  , maybeFieldView "Description: " <| dmp.dmpDescription
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
            ]
          Nothing -> [text "Error: expected DMP to have an id"]
      Loading -> [text "Loading data management plan..."]
      Error e -> [text <| "Error: " ++ e]
  }
