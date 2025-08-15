module Views.DmpEditor exposing (..)

import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (Html)
import DmpApi exposing (newDmp, editDmp)
import Models exposing (..)
import Utils exposing (..)
import Platform.Cmd as Cmd
import Html exposing (div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Html exposing (input)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Browser.Navigation as Nav
import Array
import Html.Attributes exposing (class)
import Html exposing (label)
import Html.Attributes exposing (id)
import Html.Attributes exposing (for)
import List exposing (map)
import Platform.Cmd as Cmd
import Maybe exposing (withDefault)
import Html exposing (h3)
import Html exposing (h4)
import Html exposing (h6)
import Html exposing (h5)
import Html exposing (h2)
import Html exposing (select)
import Html exposing (option)
import Html exposing (hr)
import Html.Attributes exposing (selected)
import User
import Html.Attributes exposing (type_)
import Html.Attributes exposing (checked)
import Html.Events exposing (onCheck)
import Config exposing (Config)
import Http

type ModelStatus = Editing | Submitting | SubmitError Http.Error | NotLoggedInError

type EditorMode = New | Edit String

type alias Model =
  { dmp: Dmp
  , status: ModelStatus
  , key: Nav.Key
  , mode: EditorMode
  , session: User.LoginSession
  }

type ModifyContactIdMsg
  = ModifyContactIdIdentifier (Maybe String)
  | ModifyContactIdType PersonIdType

type ModifyContactMsg
  = ModifyContactMbox String
  | ModifyContactName String
  | ModifyContactOrganization (Maybe String)
  | ModifyContactContactId ModifyContactIdMsg

type ModifyDmpIdMsg
  = ModifyDmpIdIdentifier (Maybe String)
  | ModifyDmpIdType DocumentIdType

type ModifyContributorIdMsg
  = ModifyContributorIdIdentifier (Maybe String)
  | ModifyContributorIdType PersonIdType

type ModifyContributorMsg
  = ModifyContributorMbox (Maybe String)
  | ModifyContributorName String
  | ModifyContributorOrganization (Maybe String)
  | ModifyContributorRole RoleType
  | ModifyContributorId ModifyContributorIdMsg

type ModifyDataLifeCycleMsg
  = ModifyDataLifeCycleArchivingServicesData Bool
  | ModifyDataLifeCycleBackupData String
  | ModifyDataLifeCycleDeletionData DeletionDataType
  | ModifyDataLifeCycleDeletionWhenData (Maybe Day)

type ModifyDatasetIdMsg
  = ModifyDatasetIdIdentifier (Maybe String)
  | ModifyDatasetIdType DocumentIdType

type ModifyLicenseMsg
  = ModifyLicenseRef String
  | ModifyLicenseStartDate Day

type ModifyDistributionMsg
  = ModifyDistributionAccessUrl (Maybe String)
  | ModifyDistributionDataAccess (Maybe DataAccessType)
  | ModifyDistributionDescription (Maybe String)
  | ModifyDistributionDownloadUri (Maybe String)
  | ModifyDistributionFormat (Maybe String)
  | ModifyDistributionTitle String
  | ModifyDistributionLicense Int ModifyLicenseMsg
  | AddDistributionLicense
  | RemoveDistributionLicense Int

type ModifyMetadataMsg
    = ModifyMetadataAccessDocumentation (Maybe Bool)
    | ModifyMetadataDataModel (Maybe String)
    | ModifyMetadataDescription (Maybe String)
    | ModifyMetadataLanguage LanguageType
    | ModifyMetadataLocationDocumentation (Maybe String)
    | ModifyMetadataOpen (Maybe Bool)
    | ModifyMetadataLocation (Maybe String)
    | ModifyMetadataSchema (Maybe Bool)
    | ModifyMetadataMetadataId ModifyMetadataIdMsg

type ModifyMetadataIdMsg
    = ModifyMetadataIdIdentifier (Maybe String)
    | ModifyMetadataIdType MetadataIdType

type ModifyRightsMsg
    = ModifyRightsOwnershipDataRight (Maybe String)

type ModifySecurityMsg
    = ModifySecurityDescription String
    | ModifySecurityTitle String

type ModifyDatasetMsg
  = ModifyDatasetDataQualityAssurance (Maybe String)
  | ModifyDatasetDataSharingIssues (Maybe String)
  | ModifyDatasetDescription (Maybe String)
  | ModifyDatasetIssued (Maybe Day)
  | ModifyDatasetKeywords Int String
  | AddDatasetKeyword
  | RemoveDatasetKeyword Int
  | ModifyDatasetLanguage (Maybe LanguageType)
  | ModifyDatasetPersonalData PersonalDataType
  | ModifyDatasetSensitiveData SensitiveDataType
  | ModifyDatasetReuseDataset (Maybe Bool)
  | ModifyDatasetTitle String
  | ModifyDatasetType (Maybe String)
  | ModifyDatasetDatasetId ModifyDatasetIdMsg
  | ModifyDatasetDistribution Int ModifyDistributionMsg
  | AddDatasetDistribution
  | RemoveDatasetDistribution Int
  | ModifyDatasetMetadata Int ModifyMetadataMsg
  | AddDatasetMetadata
  | RemoveDatasetMetadata Int
  | ModifyDatasetRights Int ModifyRightsMsg
  | AddDatasetRights
  | RemoveDatasetRights Int
  | ModifyDatasetSecurity Int ModifySecurityMsg
  | AddDatasetSecurity
  | RemoveDatasetSecurity Int

type ModifyEthicalIssueMsg
    = ModifyEthicalIssueDescription (Maybe String)
    | ModifyEthicalIssueExist EthicalIssuesType
    | ModifyEthicalIssueReport (Maybe String)

type ModifyProjectMsg
    = ModifyProjectDescription String
    | ModifyProjectEndDate (Maybe Day)
    | ModifyProjectStartDate Day
    | ModifyProjectTitle String

type ModifyDmpMsg
  = ModifyDmpOrgId String
  | ModifyDmpDescription (Maybe String)
  | ModifyDmpLanguage LanguageType
  | ModifyDmpNextReviewDmp (Maybe Day)
  | ModifyDmpTitle String
  | ModifyDmpTypeDmp DmpType
  | ModifyDmpContact ModifyContactMsg
  | ModifyDmpDmpId ModifyDmpIdMsg
  | ModifyDmpContributor Int ModifyContributorMsg
  | AddDmpContributor
  | RemoveDmpContributor Int
  | ModifyDmpDataLifeCycle Int ModifyDataLifeCycleMsg
  | AddDmpDataLifeCycle
  | RemoveDmpDataLifeCycle Int
  | ModifyDmpDataset Int ModifyDatasetMsg
  | AddDmpDataset
  | RemoveDmpDataset Int
  | ModifyDmpEthicalIssue Int ModifyEthicalIssueMsg
  | AddDmpEthicalIssue
  | RemoveDmpEthicalIssue Int
  | ModifyDmpProject Int ModifyProjectMsg
  | AddDmpProject
  | RemoveDmpProject Int

type Msg
  = OnSubmit
  | GotDmpApiResponse (Result Http.Error String)
  | OnModifyDmp ModifyDmpMsg

defaultContributor : Contributor
defaultContributor =
  { contributorMbox = Nothing
  , contributorName = ""
  , contributorOrganization = Nothing
  , contributorRole = RoleTypeDataController
  , contributorContributorId =
    { contributorIdIdentifier = Nothing
    , contributorIdType = PersonIdTypeNone
    }
  }

defaultLicense : License
defaultLicense =
  { licenseRef = ""
  , licenseStartDate = Day ""
  }

defaultDistribution : Distribution
defaultDistribution =
  { distributionAccessUrl = Nothing
  , distributionDataAccess = Nothing
  , distributionDescription = Nothing
  , distributionDownloadUri = Nothing
  , distributionFormat = Nothing
  , distributionTitle = ""
  , distributionLicenses = Array.empty
  }

defaultMetadata : Metadata
defaultMetadata =
  { metadataAccessDocumentation = Nothing
  , metadataDataModel = Nothing
  , metadataDescription = Nothing
  , metadataLanguage = LanguageTypeFi
  , metadataLocationDocumentation = Nothing
  , metadataOpen = Nothing
  , metadataLocation = Nothing
  , metadataSchema = Nothing
  , metadataMetadataId = { metadataIdIdentifier = Nothing, metadataIdType = MetadataIdTypeNone }
  }

defaultRights : RightsRelatedToData
defaultRights =
  { rightsOwnershipDataRight = Nothing
  }

defaultSecurity : SecurityAndPrivacy
defaultSecurity =
  { securityDescription = ""
  , securityTitle = ""
  }

defaultDataset : Dataset
defaultDataset =
  { datasetDataQualityAssurance = Nothing
  , datasetDataSharingIssues = Nothing
  , datasetDescription = Nothing
  , datasetIssued = Nothing
  , datasetKeywords = Nothing
  , datasetLanguage = Nothing
  , datasetPersonalData = PersonalDataTypeUnknown
  , datasetSensitiveData = SensitiveDataTypeUnknown
  , datasetReuseDataset = Nothing
  , datasetTitle = ""
  , datasetType = Nothing
  , datasetDatasetId = { datasetIdIdentifier = Nothing, datasetIdType = DocumentIdTypeNone }
  , datasetDistributions = Array.empty
  , datasetMetadata = Array.empty
  , datasetRightsRelatedToData = Array.empty
  , datasetSecurityAndPrivacy = Array.empty
  }

defaultDataLifeCycle : DataLifeCycle
defaultDataLifeCycle =
  { dataLifeCycleArchivingServicesData = True
  , dataLifeCycleBackupData = ""
  , dataLifeCycleDeletionData = DeletionDataTypeUnknown
  , dataLifeCycleDeletionWhenData = Nothing
  }

defaultEthicalIssue : EthicalIssue
defaultEthicalIssue =
  { ethicalIssueDescription = Nothing
  , ethicalIssueExist = EthicalIssuesTypeUnknown
  , ethicalIssueReport = Nothing
  }

defaultProject : Project
defaultProject =
  { projectDescription = ""
  , projectEndDate = Nothing
  , projectStartDate = Day ""
  , projectTitle = ""
  }

defaultDmp : String -> Dmp
defaultDmp org =
  { dmpId = Nothing
  , dmpCreated = Nothing
  , dmpDescription = Nothing
  , dmpLanguage = LanguageTypeFi
  , dmpModified = Nothing
  , dmpNextReviewDmp = Nothing
  , dmpOrgId = org
  , dmpTitle = ""
  , dmpTypeDmp = DmpTypeOrganizational
  , dmpContact =
    { contactMbox = ""
    , contactName = ""
    , contactContactId = { contactIdType = PersonIdTypeNone, contactIdIdentifier = Nothing }
    , contactOrganization = Nothing
    }
  , dmpDmpId = { dmpIdType = DocumentIdTypeNone, dmpIdIdentifier = Nothing }
  , dmpContributors = Array.empty
  , dmpDataLifeCycles = Array.empty
  , dmpDatasets = Array.empty
  , dmpEthicalIssues = Array.empty
  , dmpProjects = Array.empty
  }

init : Nav.Key -> EditorMode -> User.LoginSession -> ( Model, Cmd Msg )
init key mode session =
  let
    org = case session of
      User.LoggedIn _ person -> case Array.get 0 person.organisation of
        Just o -> o
        Nothing -> ""
      _ -> ""
  in
    ( { dmp = defaultDmp org
      , status = Editing
      , key = key
      , mode = mode
      , session = session
      }
    , Cmd.none
    )

removeAt : Int -> Array.Array a -> Array.Array a
removeAt idx array =
  array
    |> Array.toList
    |> List.indexedMap (\i value -> (i, value))
    |> List.filter (\(i, _) -> i /= idx)
    |> List.map Tuple.second
    |> Array.fromList

updateAt : Int -> (a -> a) -> Array.Array a -> Array.Array a
updateAt index fn arr =
  case Array.get index arr of
    Just value ->
      Array.set index (fn value) arr
    Nothing ->
      arr

updateContactId : ModifyContactIdMsg -> ContactId -> ContactId
updateContactId msg val = case msg of
  ModifyContactIdIdentifier v -> { val | contactIdIdentifier = v }
  ModifyContactIdType v -> { val | contactIdType = v }

updateContact : ModifyContactMsg -> Contact -> Contact
updateContact msg val = case msg of
  ModifyContactMbox str -> { val | contactMbox = str }
  ModifyContactName str -> { val | contactName = str }
  ModifyContactOrganization maybeStr -> { val | contactOrganization = maybeStr }
  ModifyContactContactId m -> { val | contactContactId = updateContactId m val.contactContactId }

updateDmpId : ModifyDmpIdMsg -> DmpId -> DmpId
updateDmpId msg val = case msg of
  ModifyDmpIdIdentifier v -> { val | dmpIdIdentifier = v }
  ModifyDmpIdType v -> { val | dmpIdType = v }

updateContributorId : ModifyContributorIdMsg -> ContributorId -> ContributorId
updateContributorId msg val = case msg of
  ModifyContributorIdIdentifier v -> { val | contributorIdIdentifier = v }
  ModifyContributorIdType v -> { val | contributorIdType = v }

updateContributor : ModifyContributorMsg -> Contributor -> Contributor
updateContributor msg val = case msg of
  ModifyContributorMbox v -> { val | contributorMbox = v }
  ModifyContributorName v -> { val | contributorName = v }
  ModifyContributorOrganization v -> { val | contributorOrganization = v }
  ModifyContributorRole v -> { val | contributorRole = v }
  ModifyContributorId v -> { val | contributorContributorId = updateContributorId v val.contributorContributorId }

updateDataLifeCycle : ModifyDataLifeCycleMsg -> DataLifeCycle -> DataLifeCycle
updateDataLifeCycle msg val = case msg of
  ModifyDataLifeCycleArchivingServicesData v -> { val | dataLifeCycleArchivingServicesData = v }
  ModifyDataLifeCycleBackupData v -> { val | dataLifeCycleBackupData = v }
  ModifyDataLifeCycleDeletionData v -> { val | dataLifeCycleDeletionData = v }
  ModifyDataLifeCycleDeletionWhenData v -> { val | dataLifeCycleDeletionWhenData = v }

updateDatasetId : ModifyDatasetIdMsg -> DatasetId -> DatasetId
updateDatasetId msg val = case msg of
  ModifyDatasetIdIdentifier v -> { val | datasetIdIdentifier = v }
  ModifyDatasetIdType v -> { val | datasetIdType = v }

updateLicense : ModifyLicenseMsg -> License -> License
updateLicense msg val = case msg of
  ModifyLicenseRef v -> { val | licenseRef = v }
  ModifyLicenseStartDate v -> { val | licenseStartDate = v }

updateDistribution : ModifyDistributionMsg -> Distribution -> Distribution
updateDistribution msg val = case msg of
  ModifyDistributionAccessUrl v -> { val | distributionAccessUrl = v }
  ModifyDistributionDataAccess v -> { val | distributionDataAccess = v }
  ModifyDistributionDescription v -> { val | distributionDescription = v }
  ModifyDistributionDownloadUri v -> { val | distributionDownloadUri = v }
  ModifyDistributionFormat v -> { val | distributionFormat = v }
  ModifyDistributionTitle v -> { val | distributionTitle = v }

  ModifyDistributionLicense idx v -> { val | distributionLicenses = updateAt idx (updateLicense v) val.distributionLicenses }
  AddDistributionLicense -> { val | distributionLicenses = Array.push defaultLicense val.distributionLicenses }
  RemoveDistributionLicense idx -> { val | distributionLicenses = removeAt idx val.distributionLicenses }

updateMetadataId : ModifyMetadataIdMsg -> MetadataId -> MetadataId
updateMetadataId msg val = case msg of
  ModifyMetadataIdIdentifier v -> { val | metadataIdIdentifier = v }
  ModifyMetadataIdType v -> { val | metadataIdType = v }

updateMetadata : ModifyMetadataMsg -> Metadata -> Metadata
updateMetadata msg val = case msg of
  ModifyMetadataAccessDocumentation v -> { val | metadataAccessDocumentation = v }
  ModifyMetadataDataModel v -> { val | metadataDataModel = v }
  ModifyMetadataDescription v -> { val | metadataDescription = v }
  ModifyMetadataLanguage v -> { val | metadataLanguage = v }
  ModifyMetadataLocationDocumentation v -> { val | metadataLocationDocumentation = v }
  ModifyMetadataOpen v -> { val | metadataOpen = v }
  ModifyMetadataLocation v -> { val | metadataLocation = v }
  ModifyMetadataSchema v -> { val | metadataSchema = v }
  ModifyMetadataMetadataId v -> { val | metadataMetadataId = updateMetadataId v val.metadataMetadataId }

updateRights : ModifyRightsMsg -> RightsRelatedToData -> RightsRelatedToData
updateRights msg val = case msg of
  ModifyRightsOwnershipDataRight v -> { val | rightsOwnershipDataRight = v }

updateSecurity : ModifySecurityMsg -> SecurityAndPrivacy -> SecurityAndPrivacy
updateSecurity msg val = case msg of
  ModifySecurityDescription v -> { val | securityDescription = v }
  ModifySecurityTitle v -> { val | securityTitle = v }

updateDataset : ModifyDatasetMsg -> Dataset -> Dataset
updateDataset msg val = case msg of
  ModifyDatasetDataQualityAssurance v -> { val | datasetDataQualityAssurance = v }
  ModifyDatasetDataSharingIssues v -> { val | datasetDataSharingIssues = v }
  ModifyDatasetDescription v -> { val | datasetDescription = v }
  ModifyDatasetIssued v -> { val | datasetIssued = v }
  ModifyDatasetLanguage v -> { val | datasetLanguage = v }
  ModifyDatasetPersonalData v -> { val | datasetPersonalData = v }
  ModifyDatasetSensitiveData v -> { val | datasetSensitiveData = v }
  ModifyDatasetReuseDataset v -> { val | datasetReuseDataset = v }
  ModifyDatasetTitle v -> { val | datasetTitle = v }
  ModifyDatasetType v -> { val | datasetType = v }
  ModifyDatasetDatasetId v -> { val | datasetDatasetId = updateDatasetId v val.datasetDatasetId }

  ModifyDatasetKeywords idx v -> { val | datasetKeywords = Maybe.map (updateAt idx (\_ -> v)) val.datasetKeywords }
  AddDatasetKeyword -> { val | datasetKeywords = Maybe.map (Array.push "") val.datasetKeywords }
  RemoveDatasetKeyword idx -> { val | datasetKeywords = Maybe.map (removeAt idx) val.datasetKeywords }

  ModifyDatasetDistribution idx v -> { val | datasetDistributions = updateAt idx (updateDistribution v) val.datasetDistributions }
  AddDatasetDistribution -> { val | datasetDistributions = Array.push defaultDistribution val.datasetDistributions }
  RemoveDatasetDistribution idx -> { val | datasetDistributions = removeAt idx val.datasetDistributions }

  ModifyDatasetMetadata idx v -> { val | datasetMetadata = updateAt idx (updateMetadata v) val.datasetMetadata }
  AddDatasetMetadata -> { val | datasetMetadata = Array.push defaultMetadata val.datasetMetadata }
  RemoveDatasetMetadata idx -> { val | datasetMetadata = removeAt idx val.datasetMetadata }

  ModifyDatasetRights idx v -> { val | datasetRightsRelatedToData = updateAt idx (updateRights v) val.datasetRightsRelatedToData }
  AddDatasetRights -> { val | datasetRightsRelatedToData = Array.push defaultRights val.datasetRightsRelatedToData }
  RemoveDatasetRights idx -> { val | datasetRightsRelatedToData = removeAt idx val.datasetRightsRelatedToData }

  ModifyDatasetSecurity idx v -> { val | datasetSecurityAndPrivacy = updateAt idx (updateSecurity v) val.datasetSecurityAndPrivacy }
  AddDatasetSecurity -> { val | datasetSecurityAndPrivacy = Array.push defaultSecurity val.datasetSecurityAndPrivacy }
  RemoveDatasetSecurity idx -> { val | datasetSecurityAndPrivacy = removeAt idx val.datasetSecurityAndPrivacy }

updateEthicalIssue : ModifyEthicalIssueMsg -> EthicalIssue -> EthicalIssue
updateEthicalIssue msg val = case msg of
  ModifyEthicalIssueDescription v -> { val | ethicalIssueDescription = v }
  ModifyEthicalIssueExist v -> { val | ethicalIssueExist = v }
  ModifyEthicalIssueReport v -> { val | ethicalIssueReport = v }

updateProject : ModifyProjectMsg -> Project -> Project
updateProject msg val = case msg of
  ModifyProjectDescription v -> { val | projectDescription = v }
  ModifyProjectEndDate v -> { val | projectEndDate = v }
  ModifyProjectStartDate v -> { val | projectStartDate = v }
  ModifyProjectTitle v -> { val | projectTitle = v }

updateDmp : ModifyDmpMsg -> Dmp -> Dmp
updateDmp msg dmp = case msg of
  ModifyDmpDescription v -> { dmp | dmpDescription = v }
  ModifyDmpLanguage v -> { dmp | dmpLanguage = v }
  ModifyDmpNextReviewDmp v -> { dmp | dmpNextReviewDmp = v }
  ModifyDmpOrgId v -> { dmp | dmpOrgId = v }
  ModifyDmpTitle v -> { dmp | dmpTitle = v }
  ModifyDmpTypeDmp v -> { dmp | dmpTypeDmp = v }
  ModifyDmpContact v -> { dmp | dmpContact = updateContact v dmp.dmpContact }
  ModifyDmpDmpId v -> { dmp | dmpDmpId = updateDmpId v dmp.dmpDmpId }

  ModifyDmpContributor idx v -> { dmp | dmpContributors = updateAt idx (updateContributor v) dmp.dmpContributors }
  AddDmpContributor -> { dmp | dmpContributors = Array.push defaultContributor dmp.dmpContributors }
  RemoveDmpContributor idx -> { dmp | dmpContributors = removeAt idx dmp.dmpContributors }

  ModifyDmpDataLifeCycle idx v -> { dmp | dmpDataLifeCycles = updateAt idx (updateDataLifeCycle v) dmp.dmpDataLifeCycles }
  AddDmpDataLifeCycle -> { dmp | dmpDataLifeCycles = Array.push defaultDataLifeCycle dmp.dmpDataLifeCycles }
  RemoveDmpDataLifeCycle idx -> { dmp | dmpDataLifeCycles = removeAt idx dmp.dmpDataLifeCycles }

  ModifyDmpDataset idx v -> { dmp | dmpDatasets = updateAt idx (updateDataset v) dmp.dmpDatasets }
  AddDmpDataset -> { dmp | dmpDatasets = Array.push defaultDataset dmp.dmpDatasets }
  RemoveDmpDataset idx -> { dmp | dmpDatasets = removeAt idx dmp.dmpDatasets }

  ModifyDmpEthicalIssue idx v -> { dmp | dmpEthicalIssues = updateAt idx (updateEthicalIssue v) dmp.dmpEthicalIssues }
  AddDmpEthicalIssue -> { dmp | dmpEthicalIssues = Array.push defaultEthicalIssue dmp.dmpEthicalIssues }
  RemoveDmpEthicalIssue idx -> { dmp | dmpEthicalIssues = removeAt idx dmp.dmpEthicalIssues }

  ModifyDmpProject idx v -> { dmp | dmpProjects = updateAt idx (updateProject v) dmp.dmpProjects }
  AddDmpProject -> { dmp | dmpProjects = Array.push defaultProject dmp.dmpProjects }
  RemoveDmpProject idx -> { dmp | dmpProjects = removeAt idx dmp.dmpProjects }

update : Config -> Msg -> Model -> (Model, Cmd Msg)
update cfg msg model =
  case msg of
    OnModifyDmp subMsg ->
      ( { model | dmp = updateDmp subMsg model.dmp }, Cmd.none )
    OnSubmit -> case model.status of
      Submitting -> (model, Cmd.none)
      _-> case model.session of
        User.LoggedIn personToken person ->
          case model.mode of
            New -> ({ model | status = Submitting }, newDmp cfg model.dmp personToken GotDmpApiResponse)
            Edit id -> ({ model | status = Submitting }, editDmp cfg id model.dmp personToken GotDmpApiResponse)
        _ -> ({ model | status = NotLoggedInError }, Cmd.none)
    GotDmpApiResponse res -> case res of
      Ok str -> 
        (model, Nav.pushUrl model.key "/dmp")
      Err e ->
        ({ model | status = SubmitError e }, Cmd.none)

languageSelect : LanguageType -> Bool -> (String -> a) -> Html a
languageSelect l d msg = select
  [ value (
    case l of
      LanguageTypeFi -> "LanguageTypeFi"
      LanguageTypeSv -> "LanguageTypeSv"
      LanguageTypeEn -> "LanguageTypeEn"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "LanguageTypeFi" ] [ text <| showLanguage LanguageTypeFi ]
  , option [ value "LanguageTypeSv" ] [ text <| showLanguage LanguageTypeSv ]
  , option [ value "LanguageTypeEn" ] [ text <| showLanguage LanguageTypeEn ]
  ]

maybeLangFromStr : String -> Maybe LanguageType
maybeLangFromStr s = case s of
  "None" -> Nothing
  _ -> Just <| langFromStr s

maybeLanguageSelect : Maybe LanguageType -> Bool -> (String -> a) -> Html a
maybeLanguageSelect l d msg = select
  [ value (
    case l of
      Just LanguageTypeFi -> "LanguageTypeFi"
      Just LanguageTypeSv -> "LanguageTypeSv"
      Just LanguageTypeEn -> "LanguageTypeEn"
      Nothing -> "None"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "None" ] [ text "Ei tiedossa" ]
  , option [ value "LanguageTypeFi" ] [ text <| showLanguage LanguageTypeFi ]
  , option [ value "LanguageTypeSv" ] [ text <| showLanguage LanguageTypeSv ]
  , option [ value "LanguageTypeEn" ] [ text <| showLanguage LanguageTypeEn ]
  ]

dmpTypeSelect : DmpType -> Bool -> (String -> a) -> Html a
dmpTypeSelect l d msg = select
  [ value (
    case l of
      DmpTypeStudent -> "DmpTypeStudent"
      DmpTypeAcademic -> "DmpTypeAcademic"
      DmpTypeNational -> "DmpTypeNational"
      DmpTypeInternational -> "DmpTypeInternational"
      DmpTypeOrganizational -> "DmpTypeOrganizational"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "DmpTypeStudent" ] [ text <| showDmpType DmpTypeStudent ]
  , option [ value "DmpTypeAcademic" ] [ text <| showDmpType DmpTypeAcademic ]
  , option [ value "DmpTypeNational" ] [ text <| showDmpType DmpTypeNational ]
  , option [ value "DmpTypeInternational" ] [ text <| showDmpType DmpTypeInternational ]
  , option [ value "DmpTypeOrganizational" ] [ text <| showDmpType DmpTypeOrganizational ]
  ]

documentIdTypeSelect : DocumentIdType -> Bool -> (String -> a) -> Html a
documentIdTypeSelect l d msg = select
  [ value (
    case l of
      DocumentIdTypeHandle -> "DocumentIdTypeHandle"
      DocumentIdTypeDoi    -> "DocumentIdTypeDoi"
      DocumentIdTypeArk    -> "DocumentIdTypeArk"
      DocumentIdTypeUrl    -> "DocumentIdTypeUrl"
      DocumentIdTypeOther  -> "DocumentIdTypeOther"
      DocumentIdTypeNone   -> "DocumentIdTypeNone"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "DocumentIdTypeHandle" ] [ text <| showDocumentIdType DocumentIdTypeHandle]
  , option [ value "DocumentIdTypeDoi"    ] [ text <| showDocumentIdType DocumentIdTypeDoi    ]
  , option [ value "DocumentIdTypeArk"    ] [ text <| showDocumentIdType DocumentIdTypeArk    ]
  , option [ value "DocumentIdTypeUrl"    ] [ text <| showDocumentIdType DocumentIdTypeUrl    ]
  , option [ value "DocumentIdTypeOther"  ] [ text <| showDocumentIdType DocumentIdTypeOther  ]
  , option [ value "DocumentIdTypeNone"   ] [ text <| showDocumentIdType DocumentIdTypeNone   ]
  ]

personIdTypeSelect : PersonIdType -> Bool -> (String -> a) -> Html a
personIdTypeSelect l d msg = select
  [ value (
    case l of
      PersonIdTypeOrcid  -> "PersonIdTypeOrcid"
      PersonIdTypeIsni   -> "PersonIdTypeIsni"
      PersonIdTypeOpenid -> "PersonIdTypeOpenid"
      PersonIdTypeOther  -> "PersonIdTypeOther"
      PersonIdTypeNone   -> "PersonIdTypeNone"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "PersonIdTypeOrcid"  ] [ text <| showPersonIdType PersonIdTypeOrcid  ]
  , option [ value "PersonIdTypeIsni"   ] [ text <| showPersonIdType PersonIdTypeIsni   ]
  , option [ value "PersonIdTypeOpenid" ] [ text <| showPersonIdType PersonIdTypeOpenid ]
  , option [ value "PersonIdTypeOther"  ] [ text <| showPersonIdType PersonIdTypeOther  ]
  , option [ value "PersonIdTypeNone"   ] [ text <| showPersonIdType PersonIdTypeNone   ]
  ]

roleTypeSelect : RoleType -> Bool -> (String -> a) -> Html a
roleTypeSelect l d msg = select
  [ value (
    case l of
      RoleTypeWorkPackageLeader     -> "RoleTypeWorkPackageLeader"
      RoleTypeDataController        -> "RoleTypeDataController"
      RoleTypePrincipleInvestigator -> "RoleTypePrincipleInvestigator"
      RoleTypeAuthorOfDataSet       -> "RoleTypeAuthorOfDataSet"
      RoleTypeOther                 -> "RoleTypeOther"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "RoleTypeWorkPackageLeader"     ] [ text <| showRoleType RoleTypeWorkPackageLeader     ]
  , option [ value "RoleTypeDataController"        ] [ text <| showRoleType RoleTypeDataController        ]
  , option [ value "RoleTypePrincipleInvestigator" ] [ text <| showRoleType RoleTypePrincipleInvestigator ]
  , option [ value "RoleTypeAuthorOfDataSet"       ] [ text <| showRoleType RoleTypeAuthorOfDataSet       ]
  , option [ value "RoleTypeOther"                 ] [ text <| showRoleType RoleTypeOther                 ]
  ]

deletionDataTypeSelect : DeletionDataType -> Bool -> (String -> a) -> Html a
deletionDataTypeSelect l d msg = select
  [ value (
    case l of
      DeletionDataTypeYes     -> "DeletionDataTypeYes"
      DeletionDataTypeNo      -> "DeletionDataTypeNo"
      DeletionDataTypeUnknown -> "DeletionDataTypeUnknown"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "DeletionDataTypeYes"     ] [ text <| showDeletionDataType DeletionDataTypeYes     ]
  , option [ value "DeletionDataTypeNo"      ] [ text <| showDeletionDataType DeletionDataTypeNo      ]
  , option [ value "DeletionDataTypeUnknown" ] [ text <| showDeletionDataType DeletionDataTypeUnknown ]
  ]

personalDataTypeSelect : PersonalDataType -> Bool -> (String -> a) -> Html a
personalDataTypeSelect l d msg = select
  [ value (
    case l of
      PersonalDataTypeYes     -> "PersonalDataTypeYes"
      PersonalDataTypeNo      -> "PersonalDataTypeNo"
      PersonalDataTypeUnknown -> "PersonalDataTypeUnknown"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "PersonalDataTypeYes"     ] [ text <| showPersonalDataType PersonalDataTypeYes     ]
  , option [ value "PersonalDataTypeNo"      ] [ text <| showPersonalDataType PersonalDataTypeNo      ]
  , option [ value "PersonalDataTypeUnknown" ] [ text <| showPersonalDataType PersonalDataTypeUnknown ]
  ]

sensitiveDataTypeSelect : SensitiveDataType -> Bool -> (String -> a) -> Html a
sensitiveDataTypeSelect l d msg = select
  [ value (
    case l of
      SensitiveDataTypeYes     -> "SensitiveDataTypeYes"
      SensitiveDataTypeNo      -> "SensitiveDataTypeNo"
      SensitiveDataTypeUnknown -> "SensitiveDataTypeUnknown"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "SensitiveDataTypeYes"     ] [ text <| showSensitiveDataType SensitiveDataTypeYes     ]
  , option [ value "SensitiveDataTypeNo"      ] [ text <| showSensitiveDataType SensitiveDataTypeNo      ]
  , option [ value "SensitiveDataTypeUnknown" ] [ text <| showSensitiveDataType SensitiveDataTypeUnknown ]
  ]

ethicalIssuesTypeSelect : EthicalIssuesType -> Bool -> (String -> a) -> Html a
ethicalIssuesTypeSelect l d msg = select
  [ value (
    case l of
      EthicalIssuesTypeYes     -> "EthicalIssuesTypeYes"
      EthicalIssuesTypeNo      -> "EthicalIssuesTypeNo"
      EthicalIssuesTypeUnknown -> "EthicalIssuesTypeUnknown"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "EthicalIssuesTypeYes"     ] [ text <| showEthicalIssuesType EthicalIssuesTypeYes     ]
  , option [ value "EthicalIssuesTypeNo"      ] [ text <| showEthicalIssuesType EthicalIssuesTypeNo      ]
  , option [ value "EthicalIssuesTypeUnknown" ] [ text <| showEthicalIssuesType EthicalIssuesTypeUnknown ]
  ]

metadataIdTypeSelect : MetadataIdType -> Bool -> (String -> a) -> Html a
metadataIdTypeSelect l d msg = select
  [ value (
    case l of
      MetadataIdTypeUrl   -> "MetadataIdTypeUrl"
      MetadataIdTypeOther -> "MetadataIdTypeOther"
      MetadataIdTypeNone  -> "MetadataIdTypeNone"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "MetadataIdTypeUrl"   ] [ text <| showMetadataIdType MetadataIdTypeUrl   ]
  , option [ value "MetadataIdTypeOther" ] [ text <| showMetadataIdType MetadataIdTypeOther ]
  , option [ value "MetadataIdTypeNone"  ] [ text <| showMetadataIdType MetadataIdTypeNone  ]
  ]

maybeDataAccessTypeSelect : Maybe DataAccessType -> Bool -> (String -> a) -> Html a
maybeDataAccessTypeSelect l d msg = select
  [ value (
    case l of
      Just DataAccessTypeOpen   -> "DataAccessTypeOpen"
      Just DataAccessTypeClosed -> "DataAccessTypeClosed"
      Just DataAccessTypeShared -> "DataAccessTypeShared"
      Nothing -> "undefined"
    )
  , disabled d
  , onInput msg
  ]
  [ option [ value "undefined"            ] [ text "undefined" ]
  , option [ value "DataAccessTypeOpen"   ] [ text <| showDataAccessType DataAccessTypeOpen   ]
  , option [ value "DataAccessTypeClosed" ] [ text <| showDataAccessType DataAccessTypeClosed ]
  , option [ value "DataAccessTypeShared" ] [ text <| showDataAccessType DataAccessTypeShared ]
  ]

maybeDataAccessTypeFromStr : String -> Maybe DataAccessType
maybeDataAccessTypeFromStr s = case s of
  "undefined" -> Nothing
  _ -> Just <| dataAccessTypeFromStr s

maybeBoolSelect : Maybe Bool -> Bool -> (Maybe Bool -> a) -> Html a
maybeBoolSelect v d msg = select
  [ value <| case v of
    Just True -> "true"
    Just False -> "false"
    Nothing -> "undefined"
  , disabled d
  , onInput (\str ->
    case str of
      "true" -> msg <| Just True
      "false" -> msg <| Just False
      _ -> msg <| Nothing
    )
  ]
  [ option [ value "undefined" ] [ text "Ei määritelty" ]
  , option [ value "true" ] [ text "Kyllä" ]
  , option [ value "false" ] [ text "Ei" ]
  ]

dmpIdEditorView : DmpId -> Bool -> Html Msg
dmpIdEditorView dmpId d = div []
  [ h3 [] [ text "DMP:n tunniste" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tunniste: "
      , input
        [ value <| withDefault "" dmpId.dmpIdIdentifier
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDmpId << ModifyDmpIdIdentifier << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tyyppi: "
      , documentIdTypeSelect dmpId.dmpIdType d <| OnModifyDmp << ModifyDmpDmpId << ModifyDmpIdType << documentIdTypeFromStr
      ]
    ]
  ]

datasetIdEditorView : Int -> DatasetId -> Bool -> Html Msg
datasetIdEditorView idx id d = div []
  [ h4 [] [ text "Aineiston tunniste" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tunniste: "
      , input
        [ value <| withDefault "" id.datasetIdIdentifier
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDatasetId << ModifyDatasetIdIdentifier << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tyyppi: "
      , documentIdTypeSelect id.datasetIdType d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDatasetId << ModifyDatasetIdType << documentIdTypeFromStr
      ]
    ]
  ]

licenseEditorView : Int -> Int -> Int -> License -> Bool -> Html Msg
licenseEditorView datasetIdx distributionIdx licenseIdx license d = div []
  [ h5 [] [ text "Lisenssi" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Käytetty lisenssi: "
      , input
        [ value license.licenseRef
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetDistribution distributionIdx
          << ModifyDistributionLicense licenseIdx
          << ModifyLicenseRef
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Lisenssin käyttöönottopäivä: "
      , input
        [ type_ "date"
        , value <| unwrapDay license.licenseStartDate
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetDistribution distributionIdx
          << ModifyDistributionLicense licenseIdx
          << ModifyLicenseStartDate
          << Day
        ]
        []
      ]
    ]
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset datasetIdx
      <| ModifyDatasetDistribution distributionIdx
      <| RemoveDistributionLicense licenseIdx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista lisenssi" ]
  ]

distributionEditorView : Int -> Int -> Distribution -> Bool -> Html Msg
distributionEditorView datasetIdx distributionIdx distribution d = div []
  [ h4 [] [ text "Julkaisu" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Julkaisun osoite: "
      , input
        [ value <| withDefault "" distribution.distributionAccessUrl
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionAccessUrl << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Saatavuus: "
      , maybeDataAccessTypeSelect distribution.distributionDataAccess d <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionDataAccess << maybeDataAccessTypeFromStr
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kuvaus: "
      , input
        [ value <| withDefault "" distribution.distributionDescription
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionDescription << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Latausosoite: "
      , input
        [ value <| withDefault "" distribution.distributionDownloadUri
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionDownloadUri << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tiedostotyyppi: "
      , input
        [ value <| withDefault "" distribution.distributionFormat
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionFormat << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Otsikko: "
      , input
        [ value distribution.distributionTitle
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionTitle
        ] []
      ]
    ]
  , div []
    <| Array.toList <| Array.indexedMap (\licenseIdx license -> licenseEditorView datasetIdx distributionIdx licenseIdx license d) distribution.distributionLicenses
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset datasetIdx
      <| ModifyDatasetDistribution distributionIdx
      <| AddDistributionLicense
    , disabled d
    , class "btn"
    ]
    [ text "+ Lisää lisenssi" ]
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset datasetIdx
      <| RemoveDatasetDistribution distributionIdx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista julkaisu" ]
  ]

metadataIdEditorView : Int -> Int -> MetadataId -> Bool -> Html Msg
metadataIdEditorView datasetIdx metadataIdx metadataId d = div []
  [ h5 [] [ text "Metadatan tunniste" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tunniste: "
      , input
        [ value <| withDefault "" metadataId.metadataIdIdentifier
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetMetadata metadataIdx
          << ModifyMetadataMetadataId
          << ModifyMetadataIdIdentifier
          << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tyyppi: "
      , metadataIdTypeSelect metadataId.metadataIdType d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataMetadataId
        << ModifyMetadataIdType
        << metadataIdTypeFromStr
      ]
    ]
  ]

metadataEditorView : Int -> Int -> Metadata -> Bool -> Html Msg
metadataEditorView datasetIdx metadataIdx metadata d = div []
  [ h4 [] [ text "Metadata" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Aineiston uudelleenkäyttö: "
      , maybeBoolSelect metadata.metadataAccessDocumentation d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataAccessDocumentation
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Datamalli: "
      , input
        [ value <| withDefault "" metadata.metadataDataModel
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetMetadata metadataIdx
          << ModifyMetadataDataModel
          << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kuvaus: "
      , input
        [ value <| withDefault "" metadata.metadataDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetMetadata metadataIdx
          << ModifyMetadataDescription
          << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kieli: "
      , languageSelect metadata.metadataLanguage d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataLanguage << langFromStr
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Dokumentaation sijainti: "
      , input
        [ value <| withDefault "" metadata.metadataLocationDocumentation
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetMetadata metadataIdx
          << ModifyMetadataLocationDocumentation
          << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Metadatan avoimuus: "
      , maybeBoolSelect metadata.metadataOpen d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataOpen
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Metadatan osoite: "
      , input
        [ value <| withDefault "" metadata.metadataLocation
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetMetadata metadataIdx
          << ModifyMetadataLocation
          << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Metadata perustuu tietomalliin: "
      , maybeBoolSelect metadata.metadataSchema d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataSchema
      ]
    ]
  , metadataIdEditorView datasetIdx metadataIdx metadata.metadataMetadataId d
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset datasetIdx
      <| RemoveDatasetMetadata metadataIdx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista metadata-osio" ]
  ]

rightsEditorView : Int -> Int -> RightsRelatedToData -> Bool -> Html Msg
rightsEditorView datasetIdx rightsIdx rights d = div []
  [ h4 [] [ text "Datan oikeudet" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Datan omistaja: "
      , input
        [ value <| withDefault "" rights.rightsOwnershipDataRight
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetRights rightsIdx
          << ModifyRightsOwnershipDataRight
          << parseMaybe
        ]
        []
      ]
    ]
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset datasetIdx
      <| RemoveDatasetRights rightsIdx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista 'datan oikeudet' -osio" ]
  ]

securityEditorView : Int -> Int -> SecurityAndPrivacy -> Bool -> Html Msg
securityEditorView datasetIdx securityIdx security d = div []
  [ h4 [] [ text "Tietoturva" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tietoturvakäytäntöjen kuvaus: "
      , input
        [ value <| security.securityDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetSecurity securityIdx
          << ModifySecurityDescription
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Otsikko: "
      , input
        [ value <| security.securityTitle
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetSecurity securityIdx
          << ModifySecurityTitle
        ]
        []
      ]
    ]
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset datasetIdx
      <| RemoveDatasetSecurity securityIdx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista tietoturva-osio" ]
  ]

contactIdEditorView : ContactId -> Bool -> Html Msg
contactIdEditorView c d = div []
  [ h4 [] [ text "Kontaktin tunniste" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tunniste: "
      , input
        [ value <| withDefault "" c.contactIdIdentifier
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactContactId << ModifyContactIdIdentifier << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tyyppi: "
      , personIdTypeSelect c.contactIdType d <| OnModifyDmp << ModifyDmpContact << ModifyContactContactId << ModifyContactIdType << personIdTypeFromStr
      ]
    ]
  ]

contactEditorView : Contact -> Bool -> Html Msg
contactEditorView c d = div []
  [ h3 [] [ text "Kontakti" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Sähköpostiosoite: "
      , input
        [ value c.contactMbox
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactMbox
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Nimi: "
      , input
        [ value c.contactName
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactName
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Organisaatio: "
      , input
        [ value <| withDefault "" c.contactOrganization
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactOrganization << parseMaybe
        ] []
      ]
    ]
  , contactIdEditorView c.contactContactId d
  ]

contributorIdEditorView : Int -> ContributorId -> Bool -> Html Msg
contributorIdEditorView idx c d = div []
  [ h4 [] [ text "Osallistujan tunniste" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tunniste: "
      , input
        [ value <| withDefault "" c.contributorIdIdentifier
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorId << ModifyContributorIdIdentifier << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tyyppi: "
      , personIdTypeSelect c.contributorIdType d <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorId << ModifyContributorIdType << personIdTypeFromStr
      ]
    ]
  ]

contributorEditorView : Int -> Contributor -> Bool -> Html Msg
contributorEditorView idx elem d = div []
  [ h3 [] [ text "Osallistuja" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Sähköpostiosoite: "
      , input
        [ value <| withDefault "" elem.contributorMbox
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorMbox << parseMaybe
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Nimi: "
      , input
        [ value elem.contributorName
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorName
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Organisaatio: "
      , input
        [ value <| withDefault "" elem.contributorOrganization
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorOrganization << parseMaybe
        ] []
      ]
    ]
    , div [ class "form-field" ]
      [ label []
        [ text "Rooli: "
        , roleTypeSelect elem.contributorRole d <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorRole << roleTypeFromStr
        ]
      ]
  , contributorIdEditorView idx elem.contributorContributorId d
  , button
    [ onClick <| OnModifyDmp <| RemoveDmpContributor idx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista osallistuja" ]
  , hr [] []
  ]

dataLifeCycleEditorView : Int -> DataLifeCycle -> Bool -> Html Msg
dataLifeCycleEditorView idx elem d = div []
  [ h3 [] [ text "Datan elinkaari" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Arkistointi: "
      , input
        [ checked elem.dataLifeCycleArchivingServicesData
        , type_ "checkbox"
        , disabled d
        , onCheck <| OnModifyDmp << ModifyDmpDataLifeCycle idx << ModifyDataLifeCycleArchivingServicesData
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Datan varmuuskopiointi: "
      , input
        [ value elem.dataLifeCycleBackupData
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataLifeCycle idx << ModifyDataLifeCycleBackupData
        ] []
      ]
    ]
  , deletionDataTypeSelect elem.dataLifeCycleDeletionData d <| OnModifyDmp << ModifyDmpDataLifeCycle idx << ModifyDataLifeCycleDeletionData << deletionDataTypeFromStr
  , div [ class "form-field" ]
    [ label []
      [ text "Datan poistamispäivä: "
      , input
        [ type_ "date"
        , value <| withDefault "" (Maybe.map unwrapDay elem.dataLifeCycleDeletionWhenData)
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataLifeCycle idx << ModifyDataLifeCycleDeletionWhenData << Maybe.map Day << parseMaybe
        ]
        []
      ]
    ]
  ]

keywordEditorView : Int -> Int -> String -> Bool -> Html Msg
keywordEditorView datasetIdx keywordIdx keyword d = div []
  [ div [ class "form-field" ]
    [ label []
      [ text "Avainsanat: "
      , input
        [ value keyword
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetKeywords keywordIdx
        ]
        []
      ]
    ]
  , button
    [ onClick <| OnModifyDmp <| ModifyDmpDataset datasetIdx <| RemoveDatasetKeyword keywordIdx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista avainsana" ]
  ]

datasetEditorView : Int -> Dataset -> Bool -> Html Msg
datasetEditorView idx elem d = div [] [ h3 [] [ text "Aineisto" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Laadunvarmistuksen kuvaus: "
      , input
        [ value <| withDefault "" elem.datasetDataQualityAssurance
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDataQualityAssurance << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Datanjakamisen haasteet: "
      , input
        [ value <| withDefault "" elem.datasetDataSharingIssues
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDataSharingIssues << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kuvaus: "
      , input
        [ value <| withDefault "" elem.datasetDescription
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDescription << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Aineiston tuotantoajankohta: "
      , input
        [ value <| withDefault "" <| Maybe.map unwrapDay elem.datasetIssued
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetIssued << Maybe.map Day << parseMaybe
        ]
        []
      ]
    ]
    , div [] <| Array.toList
      <| Array.indexedMap (\keywordIdx keyword -> keywordEditorView idx keywordIdx keyword d) (Maybe.withDefault Array.empty elem.datasetKeywords)
  , button
    [ onClick <| OnModifyDmp <| ModifyDmpDataset idx <| AddDatasetKeyword
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "+ Lisää avainsana" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kieli: "
      , maybeLanguageSelect elem.datasetLanguage d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetLanguage << maybeLangFromStr
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Henkilötiedot: "
      , personalDataTypeSelect elem.datasetPersonalData d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetPersonalData << personalDataTypeFromStr
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Sensitiivinen data: "
      , sensitiveDataTypeSelect elem.datasetSensitiveData d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetSensitiveData << sensitiveDataTypeFromStr
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Aineiston uudelleenkäyttö: "
      , maybeBoolSelect elem.datasetReuseDataset d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetReuseDataset
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Otsikko: "
      , input
        [ value elem.datasetTitle
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetTitle
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Tyyppi: "
      , input
        [ value <| withDefault "" elem.datasetType
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetType << parseMaybe
        ]
        []
      ]
    ]
  , datasetIdEditorView idx elem.datasetDatasetId d
  , div []
    <| Array.toList <| Array.indexedMap (\i e -> distributionEditorView idx i e d) elem.datasetDistributions
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset idx
      <| AddDatasetDistribution
    , disabled d
    , class "btn"
    ]
    [ text "+ Lisää julkaisu" ]
  , div []
    <| Array.toList <| Array.indexedMap (\i e -> metadataEditorView idx i e d) elem.datasetMetadata
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset idx
      <| AddDatasetMetadata
    , disabled d
    , class "btn"
    ]
    [ text "+ Lisää metadata-osio" ]
  , div []
    <| Array.toList <| Array.indexedMap (\i e -> rightsEditorView idx i e d) elem.datasetRightsRelatedToData
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset idx
      <| AddDatasetRights
    , disabled d
    , class "btn"
    ]
    [ text "+ Lisää 'datan oikeudet' -osio" ]
  , div []
    <| Array.toList <| Array.indexedMap (\i e -> securityEditorView idx i e d) elem.datasetSecurityAndPrivacy
  , button
    [ onClick <| OnModifyDmp
      <| ModifyDmpDataset idx
      <| AddDatasetSecurity
    , disabled d
    , class "btn"
    ]
    [ text "+ Lisää tietoturva-osio" ]
  , button
    [ onClick <| OnModifyDmp <| RemoveDmpDataset idx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista aineisto" ]
  , hr [] []
  ]

ethicalIssueEditorView : Int -> EthicalIssue -> Bool -> Html Msg
ethicalIssueEditorView idx ethicalIssue d = div []
  [ h3 [] [ text "Eettiset haasteet" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kuvaus: "
      , input
        [ value <| withDefault "" ethicalIssue.ethicalIssueDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpEthicalIssue idx
          << ModifyEthicalIssueDescription
          << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Eettisiä haasteita on: "
      , ethicalIssuesTypeSelect ethicalIssue.ethicalIssueExist d <| OnModifyDmp
        << ModifyDmpEthicalIssue idx
        << ModifyEthicalIssueExist
        << ethicalIssuesTypeFromStr
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Raportti eettisistä haasteista: "
      , input
        [ value <| withDefault "" ethicalIssue.ethicalIssueReport
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpEthicalIssue idx
          << ModifyEthicalIssueReport
          << parseMaybe
        ]
        []
      ]
    ]
  , button
    [ onClick <| OnModifyDmp <| RemoveDmpEthicalIssue idx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista 'eettiset haasteet' -osio" ]
  , hr [] []
  ]

projectEditorView : Int -> Project -> Bool -> Html Msg
projectEditorView idx project d = div []
  [ h3 [] [ text "Projekti" ]
  , div [ class "form-field" ]
    [ label []
      [ text "Kuvaus: "
      , input
        [ value project.projectDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectDescription
        ] []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Projektin loppumispäivä: "
      , input
        [ type_ "date"
        , value <| withDefault "" (Maybe.map unwrapDay project.projectEndDate)
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectEndDate
          << Maybe.map Day
          << parseMaybe
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Projektin alkamispäivä: "
      , input
        [ type_ "date"
        , value <| unwrapDay project.projectStartDate
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectStartDate
          << Day
        ]
        []
      ]
    ]
  , div [ class "form-field" ]
    [ label []
      [ text "Otsikko: "
      , input
        [ value project.projectTitle
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectTitle
        ] []
      ]
    ]
  , button
    [ onClick <| OnModifyDmp <| RemoveDmpProject idx
    , disabled d
    , class "btn btn-danger"
    ]
    [ text "- Poista projekti-osio" ]
  , hr [] []
  ]

maybeFieldView : String -> Maybe String -> Html Msg
maybeFieldView prefix maybeField =
  case maybeField of
    Just str -> div [] [text <| String.append prefix <| str]
    Nothing -> text ""

dmpEditorView : Dmp -> Bool -> EditorMode -> User.LoginSession -> Html Msg
dmpEditorView dmp d mode session =
  let
    orgToOption org = option [ value org, selected <| dmp.dmpOrgId == org ] [ text org ]
  in div [ class "dmp-editor" ]
    [ h2 [] [ text "DMP - Lomake" ]
    , maybeFieldView "Luotu: " (Maybe.map showUtcTime dmp.dmpCreated)
    , maybeFieldView "Muokattu: " (Maybe.map showUtcTime dmp.dmpModified)
    , div [ class "form-field" ] <| case mode of
      Edit _ -> [ label [] [ text <| "Organisaatio: " ++ dmp.dmpOrgId ] ]
      New ->
        case session of
          User.LoggedIn personToken person ->
            [ label [ for "dmp-editor-org" ] [ text "Organisaatio" ]
            , select
              [ id "dmp-editor-org"
              , value <| case Array.get 0 person.organisation of
                Just org -> org
                Nothing -> ""
              , disabled d
              , onInput (\str -> OnModifyDmp (ModifyDmpOrgId str))
              ]
              (Array.toList <| Array.map orgToOption person.organisation)
            ]
          _ -> [ text "Kirjaudu sisään, jotta voit käsitellä DMP:itä." ]
    , div [ class "form-field" ]
      [ label []
        [ text "Otsikko: "
        , input
          [ value dmp.dmpTitle
          , disabled d
          , onInput <| OnModifyDmp << ModifyDmpTitle
          ]
          []
        ]
      ]
    , div [ class "form-field" ]
      [ label []
        [ text "Kuvaus: "
        , input
          [ value <| withDefault "" dmp.dmpDescription
          , disabled d
          , onInput <| OnModifyDmp << ModifyDmpDescription << parseMaybe
          ]
          []
        ]
      ]
    , div [ class "form-field" ]
      [ label []
        [ text "Seuraava tarkastuspäivä: "
        , input
          [ type_ "date"
          , value <| withDefault "" (Maybe.map unwrapDay dmp.dmpNextReviewDmp)
          , disabled d
          , onInput <| OnModifyDmp << ModifyDmpNextReviewDmp << Maybe.map Day << parseMaybe
          ]
          []
        ]
      ]
    , div [ class "form-field" ]
      [ label []
        [ text "Kieli: "
        , languageSelect dmp.dmpLanguage d <| OnModifyDmp << ModifyDmpLanguage << langFromStr
        ]
      ]
    , div [ class "form-field" ]
      [ label []
        [ text "Tyyppi: "
        , dmpTypeSelect dmp.dmpTypeDmp d <| OnModifyDmp << ModifyDmpTypeDmp << dmpTypeFromStr
        ]
      ]
    , hr [] []
    , dmpIdEditorView dmp.dmpDmpId d
    , hr [] []
    , contactEditorView dmp.dmpContact d
    , hr [] []
    , div []
      <| Array.toList <| Array.indexedMap (\idx elem -> contributorEditorView idx elem d) dmp.dmpContributors
    , button
      [ onClick <| OnModifyDmp AddDmpContributor
      , disabled d
      , class "btn"
      ]
      [ text "+ Lisää osallistuja" ]
    , hr [] []
    , div []
      <| Array.toList <| Array.indexedMap (\idx elem -> dataLifeCycleEditorView idx elem d) dmp.dmpDataLifeCycles
    , button
      [ onClick <| OnModifyDmp AddDmpDataLifeCycle
      , disabled d
      , class "btn"
      ]
      [ text "+ Lisää 'datan elinkaari' -osio" ]
    , hr [] []
    , div []
      <| Array.toList <| Array.indexedMap (\idx elem -> datasetEditorView idx elem d) dmp.dmpDatasets
    , button
      [ onClick <| OnModifyDmp AddDmpDataset
      , disabled d
      , class "btn"
      ]
      [ text "+ Lisää aineisto" ]
    , hr [] []
    , div []
      <| Array.toList <| Array.indexedMap (\idx elem -> ethicalIssueEditorView idx elem d) dmp.dmpEthicalIssues
    , button
      [ onClick <| OnModifyDmp AddDmpEthicalIssue
      , disabled d
      , class "btn"
      ]
      [ text "+ Lisää 'eettiset haasteet' -osio" ]
    , hr [] []
    , div []
      <| Array.toList <| Array.indexedMap (\idx elem -> projectEditorView idx elem d) dmp.dmpProjects
    , button
      [ onClick <| OnModifyDmp AddDmpProject
      , disabled d
      , class "btn"
      ]
      [ text "+ Lisää projekti-osio" ]
    , hr [] []
    ]

editorFormView : Model -> Html Msg
editorFormView model = 
  div [ class "dmp-editor-wrapper" ]
  [ div []
      [ dmpEditorView model.dmp (model.status == Submitting) model.mode model.session
      , button
        [ onClick OnSubmit
        , disabled (model.status == Submitting)
        , class "btn btn-primary"
        ]
        [ text "Tallenna" ]
      ]
  , div [] <| case model.status of
    SubmitError e -> [ text <| "Virhe DMP:tä tallennettaessa: " ++ httpErrorToString e ]
    NotLoggedInError -> [ text <| "Virhe DMP:tä tallennettaessa: Kirjautuminen puuttuu!" ]
    _ -> []
  ]

view : Model -> Html Msg
view model = editorFormView model

