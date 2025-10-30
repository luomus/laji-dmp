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
import Html exposing (section)
import Html exposing (textarea)
import Html exposing (p)
import DmpApi exposing (ErrorResponse)
import Views.Errors exposing (errorResponseView)
import Organization exposing (OrgLookup)

type ModelStatus = Editing | Submitting | SubmitError ErrorResponse | NotLoggedInError

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
  | ModifyDataLifeCycleDeletionWhenData (Maybe Day)
  | ModifyDataLifeCycleUpdateFrequency String

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
  = ModifyMetadataLanguage LanguageType
  | ModifyMetadataOpen (Maybe Bool)
  | ModifyMetadataLocation (Maybe String)
  | ModifyMetadataStandards Int String
  | AddMetadataStandard
  | RemoveMetadataStandard Int
  | ModifyMetadataMetadataId ModifyMetadataIdMsg

type ModifyMetadataIdMsg
    = ModifyMetadataIdIdentifier (Maybe String)
    | ModifyMetadataIdType MetadataIdType

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
  | ModifyDatasetLanguage LanguageType
  | ModifyDatasetPersonalData PersonalDataType
  | ModifyDatasetSensitiveData SensitiveDataType
  | ModifyDatasetReuseDataset (Maybe Bool)
  | ModifyDatasetTitle String
  | ModifyDatasetType (Maybe String)
  | ModifyDatasetVocabulary Int String
  | AddDatasetVocabulary
  | RemoveDatasetVocabulary Int
  | ModifyDatasetDatasetId ModifyDatasetIdMsg
  | ModifyDatasetDistribution Int ModifyDistributionMsg
  | AddDatasetDistribution
  | RemoveDatasetDistribution Int
  | ModifyDatasetMetadata Int ModifyMetadataMsg
  | AddDatasetMetadata
  | RemoveDatasetMetadata Int
  | ModifyDatasetSecurity Int ModifySecurityMsg
  | AddDatasetSecurity
  | RemoveDatasetSecurity Int
  | ModifyDatasetDataLifeCycle ModifyDataLifeCycleMsg
  | AddDatasetDataLifeCycle
  | RemoveDatasetDataLifeCycle

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
  | GotDmpApiResponse (Result ErrorResponse String)
  | OnModifyDmp ModifyDmpMsg

defaultContributor : Contributor
defaultContributor =
  { contributorMbox = Nothing
  , contributorName = ""
  , contributorOrganization = Nothing
  , contributorRole = RoleTypeProjectDataController
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
  { metadataLanguage = LanguageTypeFi
  , metadataOpen = Nothing
  , metadataLocation = Nothing
  , metadataStandards = Nothing
  , metadataMetadataId = { metadataIdIdentifier = Nothing, metadataIdType = MetadataIdTypeNone }
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
  , datasetLanguage = LanguageTypeFi
  , datasetPersonalData = PersonalDataTypeUnknown
  , datasetSensitiveData = SensitiveDataTypeUnknown
  , datasetReuseDataset = Nothing
  , datasetTitle = ""
  , datasetType = Nothing
  , datasetVocabulary = Nothing
  , datasetDatasetId = { datasetIdIdentifier = Nothing, datasetIdType = DocumentIdTypeNone }
  , datasetDistributions = Array.empty
  , datasetMetadata = Array.empty
  , datasetSecurityAndPrivacy = Array.empty
  , datasetDataLifeCycle = Nothing
  }

defaultDataLifeCycle : DataLifeCycle
defaultDataLifeCycle =
  { dataLifeCycleArchivingServicesData = True
  , dataLifeCycleBackupData = ""
  , dataLifeCycleDeletionWhenData = Nothing
  , dataLifeCycleUpdateFrequency = ""
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
  , dmpTypeDmp = DmpTypePriodiversityLife
  , dmpContact =
    { contactMbox = ""
    , contactName = ""
    , contactContactId = { contactIdType = PersonIdTypeNone, contactIdIdentifier = Nothing }
    , contactOrganization = Nothing
    }
  , dmpDmpId = { dmpIdType = DocumentIdTypeNone, dmpIdIdentifier = Nothing }
  , dmpContributors = Array.empty
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

pushAndPrefill : a -> Array.Array a -> Array.Array a
pushAndPrefill def arr = case Array.get (Array.length arr - 1) arr of
  Just elem -> Array.push elem arr
  Nothing -> Array.push def arr

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
  ModifyDataLifeCycleDeletionWhenData v -> { val | dataLifeCycleDeletionWhenData = v }
  ModifyDataLifeCycleUpdateFrequency v -> { val | dataLifeCycleUpdateFrequency = v }

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
  AddDistributionLicense -> { val | distributionLicenses = pushAndPrefill defaultLicense val.distributionLicenses }
  RemoveDistributionLicense idx -> { val | distributionLicenses = removeAt idx val.distributionLicenses }

updateMetadataId : ModifyMetadataIdMsg -> MetadataId -> MetadataId
updateMetadataId msg val = case msg of
  ModifyMetadataIdIdentifier v -> { val | metadataIdIdentifier = v }
  ModifyMetadataIdType v -> { val | metadataIdType = v }

updateMetadata : ModifyMetadataMsg -> Metadata -> Metadata
updateMetadata msg val = case msg of
  ModifyMetadataLanguage v -> { val | metadataLanguage = v }
  ModifyMetadataOpen v -> { val | metadataOpen = v }
  ModifyMetadataLocation v -> { val | metadataLocation = v }

  ModifyMetadataStandards idx v -> { val | metadataStandards = Maybe.map (updateAt idx (\_ -> v)) val.metadataStandards }
  AddMetadataStandard ->
    { val | metadataStandards = case val.metadataStandards of
      Just k -> Just <| Array.push "" k
      Nothing -> Just <| Array.fromList [""]
    }
  RemoveMetadataStandard idx -> { val | metadataStandards = Maybe.map (removeAt idx) val.metadataStandards }

  ModifyMetadataMetadataId v -> { val | metadataMetadataId = updateMetadataId v val.metadataMetadataId }

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
  AddDatasetKeyword ->
    { val | datasetKeywords = case val.datasetKeywords of
      Just k -> Just <| Array.push "" k
      Nothing -> Just <| Array.fromList [""]
    }
  RemoveDatasetKeyword idx -> { val | datasetKeywords = Maybe.map (removeAt idx) val.datasetKeywords }

  ModifyDatasetVocabulary idx v -> { val | datasetVocabulary = Maybe.map (updateAt idx (\_ -> v)) val.datasetVocabulary }
  AddDatasetVocabulary ->
    { val | datasetVocabulary = case val.datasetVocabulary of
      Just k -> Just <| Array.push "" k
      Nothing -> Just <| Array.fromList [""]
    }
  RemoveDatasetVocabulary idx -> { val | datasetVocabulary = Maybe.map (removeAt idx) val.datasetVocabulary }

  ModifyDatasetDataLifeCycle v -> { val | datasetDataLifeCycle = Maybe.map (updateDataLifeCycle v) val.datasetDataLifeCycle }
  AddDatasetDataLifeCycle -> { val | datasetDataLifeCycle = Just defaultDataLifeCycle }
  RemoveDatasetDataLifeCycle -> { val | datasetDataLifeCycle = Nothing }

  ModifyDatasetDistribution idx v -> { val | datasetDistributions = updateAt idx (updateDistribution v) val.datasetDistributions }
  AddDatasetDistribution -> { val | datasetDistributions = pushAndPrefill defaultDistribution val.datasetDistributions }
  RemoveDatasetDistribution idx -> { val | datasetDistributions = removeAt idx val.datasetDistributions }

  ModifyDatasetMetadata idx v -> { val | datasetMetadata = updateAt idx (updateMetadata v) val.datasetMetadata }
  AddDatasetMetadata -> { val | datasetMetadata = pushAndPrefill defaultMetadata val.datasetMetadata }
  RemoveDatasetMetadata idx -> { val | datasetMetadata = removeAt idx val.datasetMetadata }

  ModifyDatasetSecurity idx v -> { val | datasetSecurityAndPrivacy = updateAt idx (updateSecurity v) val.datasetSecurityAndPrivacy }
  AddDatasetSecurity -> { val | datasetSecurityAndPrivacy = pushAndPrefill defaultSecurity val.datasetSecurityAndPrivacy }
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
  AddDmpContributor -> { dmp | dmpContributors = pushAndPrefill defaultContributor dmp.dmpContributors }
  RemoveDmpContributor idx -> { dmp | dmpContributors = removeAt idx dmp.dmpContributors }

  ModifyDmpDataset idx v -> { dmp | dmpDatasets = updateAt idx (updateDataset v) dmp.dmpDatasets }
  AddDmpDataset -> { dmp | dmpDatasets = pushAndPrefill defaultDataset dmp.dmpDatasets }
  RemoveDmpDataset idx -> { dmp | dmpDatasets = removeAt idx dmp.dmpDatasets }

  ModifyDmpEthicalIssue idx v -> { dmp | dmpEthicalIssues = updateAt idx (updateEthicalIssue v) dmp.dmpEthicalIssues }
  AddDmpEthicalIssue -> { dmp | dmpEthicalIssues = pushAndPrefill defaultEthicalIssue dmp.dmpEthicalIssues }
  RemoveDmpEthicalIssue idx -> { dmp | dmpEthicalIssues = removeAt idx dmp.dmpEthicalIssues }

  ModifyDmpProject idx v -> { dmp | dmpProjects = updateAt idx (updateProject v) dmp.dmpProjects }
  AddDmpProject -> { dmp | dmpProjects = pushAndPrefill defaultProject dmp.dmpProjects }
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

type alias EnumSelectParams a msg =
  { current : a
  , options : List a
  , optionToString : (a -> String)
  , optionFromString : (String -> a)
  , optionToLabel : (a -> String)
  , msg : (a -> msg)
  , disabled : Bool
  }

enumSelect : EnumSelectParams a msg -> Html msg
enumSelect params =
  let
    mapOption : a -> Html b
    mapOption op = option
      [ value <| params.optionToString op
      , selected <| params.current == op
      ]
      [ text <| params.optionToLabel op ]
  in
    select
    [ disabled params.disabled
    , onInput <| params.msg << params.optionFromString
    ]
    <| List.map mapOption params.options

languageSelect : LanguageType -> Bool -> (LanguageType -> a) -> Html a
languageSelect curr d toMsg =
  enumSelect
    { current = curr
    , options = [ LanguageTypeFi, LanguageTypeSv, LanguageTypeEn, LanguageTypeOther ]
    , optionToString = langToStr
    , optionFromString = langFromStr
    , optionToLabel = showLanguage
    , msg = toMsg
    , disabled = d
    }

maybeLanguageSelect : Maybe LanguageType -> Bool -> (Maybe LanguageType -> a) -> Html a
maybeLanguageSelect curr d toMsg =
  let
    optionToString o = case o of
      Nothing -> "None"
      Just l -> langToStr l
    optionFromString o = case o of
      "None" -> Nothing
      str -> Just <| langFromStr str
    optionToLabel o = case o of
      Nothing -> "Ei tiedossa"
      Just l -> showLanguage l
  in
    enumSelect
      { current = curr
      , options = [ Just LanguageTypeFi, Just LanguageTypeSv, Just LanguageTypeEn, Nothing ]
      , optionToString = optionToString
      , optionFromString = optionFromString
      , optionToLabel = optionToLabel
      , msg = toMsg
      , disabled = d
      }

dmpTypeSelect : DmpType -> Bool -> (DmpType -> msg) -> Html msg
dmpTypeSelect curr d toMsg =
  enumSelect
    { current = curr
    , options = [ DmpTypePriodiversityLife, DmpTypeStudent, DmpTypeAcademic, DmpTypeNational, DmpTypeInternational, DmpTypeOrganizational ]
    , optionToString = dmpTypeToStr
    , optionFromString = dmpTypeFromStr
    , optionToLabel = showDmpType
    , msg = toMsg
    , disabled = d
    }

documentIdTypeSelect : DocumentIdType -> Bool -> (DocumentIdType -> msg) -> Html msg
documentIdTypeSelect curr d toMsg =
  enumSelect
    { current = curr
    , options = [ DocumentIdTypeHandle, DocumentIdTypeDoi, DocumentIdTypeArk, DocumentIdTypeUrl, DocumentIdTypeOther, DocumentIdTypeNone ]
    , optionToString = documentIdTypeToStr
    , optionFromString = documentIdTypeFromStr
    , optionToLabel = showDocumentIdType
    , msg = toMsg
    , disabled = d
    }

personIdTypeSelect : PersonIdType -> Bool -> (PersonIdType -> msg) -> Html msg
personIdTypeSelect curr d toMsg =
  enumSelect
    { current = curr
    , options = [ PersonIdTypeOrcid, PersonIdTypeIsni, PersonIdTypeOpenid, PersonIdTypeOther, PersonIdTypeNone ]
    , optionToString = personIdTypeToStr
    , optionFromString = personIdTypeFromStr
    , optionToLabel = showPersonIdType
    , msg = toMsg
    , disabled = d
    }

roleTypeSelect : RoleType -> Bool -> (RoleType -> msg) -> Html msg
roleTypeSelect curr d toMsg =
  enumSelect
    { current = curr
    , options = [ RoleTypeProjectDataController, RoleTypeDataOwner, RoleTypeOrganizationDataController, RoleTypeDatasetAuthor, RoleTypeOther ]
    , optionToString = roleTypeToStr
    , optionFromString = roleTypeFromStr
    , optionToLabel = showRoleType
    , msg = toMsg
    , disabled = d
    }

personalDataTypeSelect : PersonalDataType -> Bool -> (PersonalDataType -> b) -> Html b
personalDataTypeSelect curr d msg = enumSelect
  { current = curr
  , options = [ PersonalDataTypeUnknown, PersonalDataTypeYes, PersonalDataTypeNo ]
  , optionToString = personalDataTypeToStr
  , optionFromString = personalDataTypeFromStr
  , optionToLabel = showPersonalDataType
  , msg = msg
  , disabled = d
  }

sensitiveDataTypeSelect : SensitiveDataType -> Bool -> (SensitiveDataType -> msg) -> Html msg
sensitiveDataTypeSelect curr d toMsg = enumSelect
  { current = curr
  , options = [ SensitiveDataTypeUnknown, SensitiveDataTypeYes, SensitiveDataTypeNo ]
  , optionToString = sensitiveDataTypeToStr
  , optionFromString = sensitiveDataTypeFromStr
  , optionToLabel = showSensitiveDataType
  , msg = toMsg
  , disabled = d
  }

ethicalIssuesTypeSelect : EthicalIssuesType -> Bool -> (EthicalIssuesType -> msg) -> Html msg
ethicalIssuesTypeSelect curr d toMsg = enumSelect
  { current = curr
  , options = [ EthicalIssuesTypeUnknown, EthicalIssuesTypeYes, EthicalIssuesTypeNo ]
  , optionToString = ethicalIssuesTypeToStr
  , optionFromString = ethicalIssuesTypeFromStr
  , optionToLabel = showEthicalIssuesType
  , msg = toMsg
  , disabled = d
  }

metadataIdTypeSelect : MetadataIdType -> Bool -> (MetadataIdType -> msg) -> Html msg
metadataIdTypeSelect curr d toMsg = enumSelect
  { current = curr
  , options = [ MetadataIdTypeNone, MetadataIdTypeUrl, MetadataIdTypeOther ]
  , optionToString = metadataIdTypeToStr
  , optionFromString = metadataIdTypeFromStr
  , optionToLabel = showMetadataIdType
  , msg = toMsg
  , disabled = d
  }

maybeDataAccessTypeSelect : Maybe DataAccessType -> Bool -> (Maybe DataAccessType -> a) -> Html a
maybeDataAccessTypeSelect curr d toMsg = 
  let
    optionToString o = case o of
      Nothing -> "undefined"
      Just l -> dataAccessTypeToStr l
    optionFromString o = case o of
      "undefined" -> Nothing
      str -> Just <| dataAccessTypeFromStr str
    optionToLabel o = case o of
      Nothing -> "Ei määritelty"
      Just l -> showDataAccessType l
  in
    enumSelect
      { current = curr
      , options = [ Just DataAccessTypeOpen, Just DataAccessTypeShared, Just DataAccessTypeClosed, Just DataAccessTypeClassified, Just DataAccessTypeEmbargoed, Nothing ]
      , optionToString = optionToString
      , optionFromString = optionFromString
      , optionToLabel = optionToLabel
      , msg = toMsg
      , disabled = d
      }

maybeBoolSelect : Maybe Bool -> Bool -> (Maybe Bool -> a) -> Html a
maybeBoolSelect curr d toMsg = 
  let
    optionToString o = case o of
      Nothing -> "undefined"
      Just l -> boolToString l
    optionFromString o = case o of
      "undefined" -> Nothing
      str -> Just <| boolFromString str
    optionToLabel o = case o of
      Nothing -> "Ei määritelty"
      Just l -> showBool l
  in
    enumSelect
      { current = curr
      , options = [ Just True, Just False, Nothing ]
      , optionToString = optionToString
      , optionFromString = optionFromString
      , optionToLabel = optionToLabel
      , msg = toMsg
      , disabled = d
      }

dmpIdEditorView : DmpId -> Bool -> Html Msg
dmpIdEditorView dmpId d = div []
  [ inputFieldView "Tunniste: " (Just "Jos DMP:lle on jo luotu pysyväistunniste (esim. DOI), lisää se tähän.")
    <| input
      [ value <| withDefault "" dmpId.dmpIdIdentifier
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpDmpId << ModifyDmpIdIdentifier << parseMaybe
      ] []
  , inputFieldView "Tunnisteen tyyppi: " (Just "Valitse listasta tunnisteen tyyppi. Valitse 'Ei tiedossa', jos DMP:llä ei ole pysyväistunnistetta.")
    <| documentIdTypeSelect dmpId.dmpIdType d <| OnModifyDmp << ModifyDmpDmpId << ModifyDmpIdType
  ]

datasetIdEditorView : Int -> DatasetId -> Bool -> Html Msg
datasetIdEditorView idx id d = div []
  [ inputFieldView "Tunniste: " (Just "Jos aineistolle on jo luotu pysyväistunniste (esim. DOI), lisää se tähän.")
    <| input
      [ value <| withDefault "" id.datasetIdIdentifier
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDatasetId << ModifyDatasetIdIdentifier << parseMaybe
      ] []
  , inputFieldView "Tunnisteen tyyppi: " (Just "Valitse listasta tunnisteen tyyppi. Valitse 'Ei tiedossa', jos aineistolla ei ole pysyväistunnistetta.")
    <| documentIdTypeSelect id.datasetIdType d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDatasetId << ModifyDatasetIdType
  ]

licenseEditorView : Int -> Int -> Int -> License -> Bool -> Html Msg
licenseEditorView datasetIdx distributionIdx licenseIdx license d = div []
  [ div []
    [ h5 [ class "d-inline-block" ] [ text <| "Lisenssi " ++ String.fromInt (licenseIdx + 1) ]
    , button
      [ onClick <| OnModifyDmp
        <| ModifyDmpDataset datasetIdx
        <| ModifyDatasetDistribution distributionIdx
        <| RemoveDistributionLicense licenseIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  , div [ class "sub-form" ]
    [ inputFieldView "Käytetty lisenssi: " (Just "Lisää linkki käytettyyn lisenssiin, esim: https://creativecommons.org/licenses/by/4.0/")
      <| input
        [ value license.licenseRef
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetDistribution distributionIdx
          << ModifyDistributionLicense licenseIdx
          << ModifyLicenseRef
        ] []
    , inputFieldView "Lisenssin käyttöönottopäivä*: " Nothing
      <| input
        [ type_ "date"
        , value <| unwrapDay license.licenseStartDate
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetDistribution distributionIdx
          << ModifyDistributionLicense licenseIdx
          << ModifyLicenseStartDate
          << Day
        ] []
    ]
  ]

distributionEditorView : Int -> Int -> Distribution -> Bool -> Html Msg
distributionEditorView datasetIdx distributionIdx distribution d = div []
  [ div []
    [ h4 [ class "d-inline-block" ] [ text <| "Julkaisu " ++ String.fromInt (distributionIdx + 1) ]
    , button
      [ onClick <| OnModifyDmp
        <| ModifyDmpDataset datasetIdx
        <| RemoveDatasetDistribution distributionIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  , div [ class "sub-form" ]
    [ inputFieldView "Otsikko*: " Nothing
      <| input
        [ value distribution.distributionTitle
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionTitle
        ] []
    , inputFieldView "Julkaisun osoite: " (Just "Verkkosivun osoite, jossa aineisto on julkaistu.")
      <| input
        [ value <| withDefault "" distribution.distributionAccessUrl
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionAccessUrl << parseMaybe
        ] []
    , inputFieldView "Avoimuus: " (Just "Onko aineisto saatavilla avoimesti tai pyydettäessä?")
      <| maybeDataAccessTypeSelect distribution.distributionDataAccess d <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionDataAccess
    , inputFieldView "Lisätiedot: " Nothing
      <| textarea
        [ value <| withDefault "" distribution.distributionDescription
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionDescription << parseMaybe
        , class "d-block"
        ] []
    , inputFieldView "Latausosoite: " (Just "Verkkosivun osoite, josta aineiston voi ladata tiedostona.")
      <| input
        [ value <| withDefault "" distribution.distributionDownloadUri
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionDownloadUri << parseMaybe
        ] []
    , inputFieldView "Tiedostotyyppi: " (Just "Ilmoita ladattavan tiedoston formaatti esim. '.json' tai '.xlsx'.")
      <| input
        [ value <| withDefault "" distribution.distributionFormat
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDistribution distributionIdx << ModifyDistributionFormat << parseMaybe
        ] []
    , section []
      [ div []
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
        ]
    ]
  ]

metadataIdEditorView : Int -> Int -> MetadataId -> Bool -> Html Msg
metadataIdEditorView datasetIdx metadataIdx metadataId d = div []
  [ inputFieldView "Tunniste: " (Just "Lisää tähän linkki tai muu tunniste metadatan standardiin.")
    <| input
      [ value <| withDefault "" metadataId.metadataIdIdentifier
      , disabled d
      , onInput <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataMetadataId
        << ModifyMetadataIdIdentifier
        << parseMaybe
      ] []
  , inputFieldView "Tunnisteen tyyppi: " (Just "Valitse listasta metadatan standardin tunnisteen tyyppi. Valitse 'Ei tiedossa', jos et halua lisätä tunnistetta.")
    <| metadataIdTypeSelect metadataId.metadataIdType d <| OnModifyDmp
      << ModifyDmpDataset datasetIdx
      << ModifyDatasetMetadata metadataIdx
      << ModifyMetadataMetadataId
      << ModifyMetadataIdType
  ]

standardEditorView : Int -> Int -> Int -> String -> Bool -> Html Msg
standardEditorView datasetIdx metadataIdx standardIdx standard d = div [ class "form-field keyword-editor" ]
  [ label []
    [ text <| "Standardi " ++ String.fromInt standardIdx ++ ": "
    , input
      [ value standard
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetMetadata metadataIdx << ModifyMetadataStandards standardIdx
      ] []
    , button
      [ onClick <| OnModifyDmp <| ModifyDmpDataset datasetIdx << ModifyDatasetMetadata metadataIdx <| RemoveMetadataStandard standardIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  ]

metadataEditorView : Int -> Int -> Metadata -> Bool -> Html Msg
metadataEditorView datasetIdx metadataIdx metadata d = div []
  [ div []
    [ h4 [ class "d-inline-block" ] [ text <| "Metadata " ++ String.fromInt (metadataIdx + 1) ]
    , button
      [ onClick <| OnModifyDmp
        <| ModifyDmpDataset datasetIdx
        <| RemoveDatasetMetadata metadataIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  , div [ class "sub-form" ]
    [ inputFieldView "Metadatan osoite: " (Just "Linkki metadataan.")
        <| input
        [ value <| withDefault "" metadata.metadataLocation
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetMetadata metadataIdx
          << ModifyMetadataLocation
          << parseMaybe
        ] []
    , inputFieldView "Kieli: " Nothing
      <| languageSelect metadata.metadataLanguage d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataLanguage
    , inputFieldView "Metadatan avoimuus: " (Just "Ovatko metatiedot kaikille avoimesti saatavilla?")
      <| maybeBoolSelect metadata.metadataOpen d <| OnModifyDmp
        << ModifyDmpDataset datasetIdx
        << ModifyDatasetMetadata metadataIdx
        << ModifyMetadataOpen
    , section []
      [ div [] <| Array.toList
        <| Array.indexedMap (\standardIdx standard -> standardEditorView datasetIdx metadataIdx standardIdx standard d) (Maybe.withDefault Array.empty metadata.metadataStandards)
      , button
        [ onClick <| OnModifyDmp <| ModifyDmpDataset datasetIdx <| ModifyDatasetMetadata metadataIdx <| AddMetadataStandard
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää standardi" ]
      ]
    , section [] [ metadataIdEditorView datasetIdx metadataIdx metadata.metadataMetadataId d ]
    ]
  ]

securityEditorView : Int -> Int -> SecurityAndPrivacy -> Bool -> Html Msg
securityEditorView datasetIdx securityIdx security d = div []
  [ div []
    [ h4 [ class "d-inline-block" ] [ text <| "Tietoturva " ++ String.fromInt (securityIdx + 1) ]
    , button
      [ onClick <| OnModifyDmp
        <| ModifyDmpDataset datasetIdx
        <| RemoveDatasetSecurity securityIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  , div [ class "sub-form" ]
    [ inputFieldView "Tietoturvakäytännön nimi*: " (Just "Esimerkkivastaus: Henkilötietojen anonymisointi")
      <| input
        [ value <| security.securityTitle
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetSecurity securityIdx
          << ModifySecurityTitle
        ] []
    , inputFieldView "Tietoturvakäytännön kuvaus*: " (Just "Kuvaile nimettyä tietoturvakäytäntöä, jolla sensitiivistä dataa suojataan. Esimerkkivastaus: Nimet, osoitteet ja puhelinnumerot korvataan aineistoon pseudonyymeillä. Tallennuspäivät korvataan päivämääräväleillä.")
      <| input
        [ value <| security.securityDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpDataset datasetIdx
          << ModifyDatasetSecurity securityIdx
          << ModifySecurityDescription
        ] []
    ]
  ]

contactIdEditorView : ContactId -> Bool -> Html Msg
contactIdEditorView c d = div []
  [ inputFieldView "Tunniste: " (Just "Jos kontaktilla on pysyväistunniste (esim. ORCID), lisää se tähän.")
    <| input
      [ value <| withDefault "" c.contactIdIdentifier
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactContactId << ModifyContactIdIdentifier << parseMaybe
      ] []
  , inputFieldView "Tunnisteen tyyppi: " (Just "Valitse listasta tunnisteen tyyppi. Valitse 'Ei tiedossa', jos kontaktilla ei ole tunnistetta.")
    <| personIdTypeSelect c.contactIdType d <| OnModifyDmp << ModifyDmpContact << ModifyContactContactId << ModifyContactIdType
  ]

contactEditorView : Contact -> Bool -> Html Msg
contactEditorView c d = div []
  [ h3 [] [ text "Kontakti" ]
  , inputFieldView "Nimi*: " (Just "Ilmoita kontaktihenkilön tai organisaation nimi.")
    <| input
      [ value c.contactName
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactName
      ] []
  , inputFieldView "Sähköpostiosoite*: " (Just "Ilmoita kontaktihenkilön tai organisaation sähköpostiosoite.")
    <| input
      [ value c.contactMbox
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactMbox
      ] []
  , inputFieldView "Organisaatio: " Nothing
    <| input
      [ value <| withDefault "" c.contactOrganization
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpContact << ModifyContactOrganization << parseMaybe
      ] []
  , section [] [ contactIdEditorView c.contactContactId d ]
  ]

contributorIdEditorView : Int -> ContributorId -> Bool -> Html Msg
contributorIdEditorView idx c d = div []
  [ inputFieldView "Tunniste: " (Just "Jos osallistujalla on pysyväistunniste (esim. ORCID), lisää se tähän.")
    <| input
      [ value <| withDefault "" c.contributorIdIdentifier
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorId << ModifyContributorIdIdentifier << parseMaybe
      ] []
  , inputFieldView "Tunnisteen tyyppi: " (Just "Valitse listasta tunnisteen tyyppi. Valitse 'Ei tiedossa', jos osallistujalla ei ole tunnistetta.")
    <| personIdTypeSelect c.contributorIdType d <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorId << ModifyContributorIdType
  ]

contributorEditorView : Int -> Contributor -> Bool -> Html Msg
contributorEditorView idx elem d = div []
  [ div []
    [ h3 [ class "d-inline-block" ] [ text <| "Osallistuja " ++ String.fromInt (idx + 1) ]
    , button
        [ onClick <| OnModifyDmp <| RemoveDmpContributor idx
        , disabled d
        , class "btn btn-danger btn-remove"
        ]
        [ text "x" ]
      ]
  , div [ class "sub-form" ]
    [ inputFieldView "Nimi*: " (Just "Osallistujiksi lisätään kontaktin lisäksi muiden relevanttien aineistonhallintasuunnitelmaan osallistuvien henkilöiden nimet.")
      <| input
        [ value elem.contributorName
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorName
        ] []
    , inputFieldView "Sähköpostiosoite: " (Just "Ilmoita aineistonhallintasuunnitelmaan osallistuvan henkilön sähköpostiosoite.")
      <| input
        [ value <| withDefault "" elem.contributorMbox
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorMbox << parseMaybe
        ] []
    , inputFieldView "Organisaatio: " Nothing
      <| input
        [ value <| withDefault "" elem.contributorOrganization
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorOrganization << parseMaybe
        ] []
    , inputFieldView "Rooli: " (Just "Valitse listasta osallistujan roolia parhaiten kuvaava nimike.")
      <| roleTypeSelect elem.contributorRole d <| OnModifyDmp << ModifyDmpContributor idx << ModifyContributorRole
    , section [] [ contributorIdEditorView idx elem.contributorContributorId d ]
    ]
  ]

dataLifeCycleEditorView : Int -> DataLifeCycle -> Bool -> Html Msg
dataLifeCycleEditorView datasetIdx elem d = div []
  [ div []
    [ h3 [ class "d-inline-block" ] [ text <| "Datan elinkaari" ]
    , button
        [ onClick <| OnModifyDmp <| ModifyDmpDataset datasetIdx <| RemoveDatasetDataLifeCycle
        , disabled d
        , class "btn btn-danger btn-remove"
        ]
        [ text "x" ]
      ]
  , div [ class "sub-form" ]
    [ inputFieldView "Arkistointi: " (Just "Arkistoidaanko aineiston dataa, eli siirtyykö se aktiivikäytöstä erilliseen säilytykseen projektin jälkeen?")
      <| input
        [ checked elem.dataLifeCycleArchivingServicesData
        , type_ "checkbox"
        , disabled d
        , onCheck <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDataLifeCycle << ModifyDataLifeCycleArchivingServicesData
        ] []
    , inputFieldView "Datan varmuuskopiointi*: " (Just "Kuvaile, kuinka aineisto varmuuskopioidaan.")
      <| input
        [ value elem.dataLifeCycleBackupData
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDataLifeCycle << ModifyDataLifeCycleBackupData
        ] []
    , inputFieldView "Datan poistamispäivä: " (Just "Jos aineistolle on määritelty poistamispäivä, ilmoita se tähän.")
      <| input
        [ type_ "date"
        , value <| withDefault "" (Maybe.map unwrapDay elem.dataLifeCycleDeletionWhenData)
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDataLifeCycle << ModifyDataLifeCycleDeletionWhenData << Maybe.map Day << parseMaybe
        ] []
    , inputFieldView "Päivityksen tiheys*: " Nothing
      <| input
        [ value elem.dataLifeCycleUpdateFrequency
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetDataLifeCycle << ModifyDataLifeCycleUpdateFrequency
        ] []
    ]
  ]

keywordEditorView : Int -> Int -> String -> Bool -> Html Msg
keywordEditorView datasetIdx keywordIdx keyword d = div [ class "form-field keyword-editor" ]
  [ label []
    [ text <| "Avainsana " ++ String.fromInt keywordIdx ++ ": "
    , input
      [ value keyword
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetKeywords keywordIdx
      ] []
    , button
      [ onClick <| OnModifyDmp <| ModifyDmpDataset datasetIdx <| RemoveDatasetKeyword keywordIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  ]

vocabularyEditorView : Int -> Int -> String -> Bool -> Html Msg
vocabularyEditorView datasetIdx vocabularyIdx vocabulary d = div [ class "form-field keyword-editor" ]
  [ label []
    [ text <| "Sanasto " ++ String.fromInt vocabularyIdx ++ ": "
    , input
      [ value vocabulary
      , disabled d
      , onInput <| OnModifyDmp << ModifyDmpDataset datasetIdx << ModifyDatasetVocabulary vocabularyIdx
      ] []
    , button
      [ onClick <| OnModifyDmp <| ModifyDmpDataset datasetIdx <| RemoveDatasetVocabulary vocabularyIdx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  ]

datasetEditorView : Int -> Dataset -> Bool -> Html Msg
datasetEditorView idx elem d = div []
  [ div []
    [ h3 [ class "d-inline-block" ] [ text <| "Aineisto " ++ String.fromInt (idx + 1) ]
    , button
        [ onClick <| OnModifyDmp <| RemoveDmpDataset idx
        , disabled d
        , class "btn btn-danger btn-remove"
        ]
        [ text "x" ]
      ]
  , div [ class "sub-form" ]
    [ inputFieldView "Nimi*: " Nothing
      <| input
        [ value elem.datasetTitle
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetTitle
        ] []
    , inputFieldView "Kuvaus: " (Just "Kuvaile aineistoa lyhyesti, esimerkiksi perustuuko se joihinkin olemassa oleviin lähdeaineistoihin, tai uuteen aineistoon.")
      <| textarea
        [ value <| withDefault "" elem.datasetDescription
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDescription << parseMaybe
        , class "d-block"
        ] []
    , inputFieldView "Tyyppi: " (Just "Aineiston tyyppi, esim. aineisto, raakadata, paikkatietoaineisto, taulukko, tietokantapoiminta.")
      <| input
        [ value <| withDefault "" elem.datasetType
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetType << parseMaybe
        ] []
    , inputFieldView "Kieli: " Nothing
      <| languageSelect elem.datasetLanguage d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetLanguage
    , section []
      [ div [] <| Array.toList
        <| Array.indexedMap (\keywordIdx keyword -> keywordEditorView idx keywordIdx keyword d) (Maybe.withDefault Array.empty elem.datasetKeywords)
      , button
        [ onClick <| OnModifyDmp <| ModifyDmpDataset idx <| AddDatasetKeyword
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää avainsana" ]
      ]
    , inputFieldView "Aineiston tuotantoajankohta: " (Just "Ilmoita aineiston tuotantoajankohta, eli päivämäärä milloin sen tuottaminen valmistuu tai on valmistunut.")
      <| input
        [ type_ "date"
        , value <| withDefault "" (Maybe.map unwrapDay elem.datasetIssued)
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetIssued << Maybe.map Day << parseMaybe
        ] []
    , inputFieldView "Aineiston uudelleenkäyttö: " (Just "Onko aineisto kerätty jo ennen projektia?")
      <| maybeBoolSelect elem.datasetReuseDataset d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetReuseDataset
    , inputFieldView "Henkilötiedot: " (Just "Sisältääkö aineisto henkilötietoja, esim. henkilöiden nimi, henkilötunnus, sähköposti, puhelinnumero?")
      <| personalDataTypeSelect elem.datasetPersonalData d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetPersonalData
    , inputFieldView "Sensitiivinen data: " (Just "Sisältääkö aineisto sensitiivistä dataa, esim. uhanalaisten lajien paikkatietoa, tms.?")
      <| sensitiveDataTypeSelect elem.datasetSensitiveData d <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetSensitiveData
    ,  inputFieldView "Laadunvarmistuksen kuvaus: " (Just "Kirjoita tähän vapaamuotoisesti aineiston laadunvarmistuksesta, esim. validoinnista ja käytetyistä menetelmistä.")
      <| input
        [ value <| withDefault "" elem.datasetDataQualityAssurance
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDataQualityAssurance << parseMaybe
        ] []
    , inputFieldView "Datanjakamisen haasteet: " (Just "Miten datan jakamiseen liittyvät oikeudelliset ja eettiset kysymykset (esim. omistajuus, tekijänoikeudet, arkaluontoisuus) ratkaistaan, jos sellaisia on?")
      <| input
        [ value <| withDefault "" elem.datasetDataSharingIssues
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpDataset idx << ModifyDatasetDataSharingIssues << parseMaybe
        ] []
    , section []
      [ div [] <| Array.toList
        <| Array.indexedMap (\vocabularyIdx vocabulary -> vocabularyEditorView idx vocabularyIdx vocabulary d) (Maybe.withDefault Array.empty elem.datasetVocabulary)
      , button
        [ onClick <| OnModifyDmp <| ModifyDmpDataset idx <| AddDatasetVocabulary
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää sanasto" ]
      ]
    , section [] [ datasetIdEditorView idx elem.datasetDatasetId d ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\i e -> distributionEditorView idx i e d) elem.datasetDistributions
      , button
        [ onClick <| OnModifyDmp
          <| ModifyDmpDataset idx
          <| AddDatasetDistribution
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää julkaisu" ]
        ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\i e -> metadataEditorView idx i e d) elem.datasetMetadata
      , button
        [ onClick <| OnModifyDmp
          <| ModifyDmpDataset idx
          <| AddDatasetMetadata
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää metadata-osio" ]
        ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\i e -> securityEditorView idx i e d) elem.datasetSecurityAndPrivacy
      , button
        [ onClick <| OnModifyDmp
          <| ModifyDmpDataset idx
          <| AddDatasetSecurity
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää tietoturva-osio" ]
        ]
    , section []
      [ div [] <| withDefault [] (Maybe.map (\x -> [dataLifeCycleEditorView idx x d]) elem.datasetDataLifeCycle)
      , case elem.datasetDataLifeCycle of
        Nothing -> button
          [ onClick <| OnModifyDmp <| ModifyDmpDataset idx <| AddDatasetDataLifeCycle
          , disabled d
          , class "btn"
          ]
          [ text "+ Lisää 'datan elinkaari' -osio" ]
        Just _ -> text ""
      ]
    ]
  ]

ethicalIssueEditorView : Int -> EthicalIssue -> Bool -> Html Msg
ethicalIssueEditorView idx ethicalIssue d = div []
  [ div []
    [ h3 [ class "d-inline-block" ] [ text <| "Eettiset haasteet " ++ String.fromInt (idx + 1) ]
    , button
      [ onClick <| OnModifyDmp <| RemoveDmpEthicalIssue idx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  , div [ class "sub-form" ]
    [ inputFieldView "Eettisiä haasteita on: " (Just "Liittyykö aineistonhallintasuunnitelman kuvailemaan dataan eettisiä haasteita?")
        <| ethicalIssuesTypeSelect ethicalIssue.ethicalIssueExist d <| OnModifyDmp
        << ModifyDmpEthicalIssue idx
        << ModifyEthicalIssueExist
    ,  inputFieldView "Kuvaus: " (Just "Kuvaile vapaamuotoisesti eettisiä haasteita, joita aineistonhallintasuunnitelman kuvailemaan dataan liittyy.")
      <| textarea
        [ value <| withDefault "" ethicalIssue.ethicalIssueDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpEthicalIssue idx
          << ModifyEthicalIssueDescription
          << parseMaybe
        , class "d-block"
        ] []
    , inputFieldView "Raportti eettisistä haasteista: " (Just "Jos eettisistä haasteista on tehty raportti, voit lisätä linkin raporttiin.")
      <| input
        [ value <| withDefault "" ethicalIssue.ethicalIssueReport
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpEthicalIssue idx
          << ModifyEthicalIssueReport
          << parseMaybe
        ] []
    ]
  ]

projectEditorView : Int -> Project -> Bool -> Html Msg
projectEditorView idx project d = div []
  [ div []
    [ h3 [ class "d-inline-block" ] [ text <| "Projekti " ++ String.fromInt (idx + 1) ]
    , button
      [ onClick <| OnModifyDmp <| RemoveDmpProject idx
      , disabled d
      , class "btn btn-danger btn-remove"
      ]
      [ text "x" ]
    ]
  , div [ class "sub-form" ]
    [ inputFieldView "Nimi*: " (Just "Ilmoita projektin nimi, johon aineistonhallintasuunnitelma liittyy.")
      <| input
        [ value project.projectTitle
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectTitle
        ] []
    ,  inputFieldView "Kuvaus*: " (Just "Projektin kuvaus.")
      <| textarea
        [ value project.projectDescription
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectDescription
        , class "d-block"
        ] []
    , inputFieldView "Projektin alkamispäivä*: " Nothing
      <| input
        [ type_ "date"
        , value <| unwrapDay project.projectStartDate
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectStartDate
          << Day
        ] []
    , inputFieldView "Projektin loppumispäivä: " Nothing
      <| input
        [ type_ "date"
        , value <| withDefault "" (Maybe.map unwrapDay project.projectEndDate)
        , disabled d
        , onInput <| OnModifyDmp
          << ModifyDmpProject idx
          << ModifyProjectEndDate
          << Maybe.map Day
          << parseMaybe
        ] []
    ]
  ]

displayFieldView : String -> Maybe String -> Html Msg
displayFieldView prefix maybeField =
  case maybeField of
    Just str -> p [] [text <| String.append prefix <| str]
    Nothing -> text ""

inputFieldView : String -> Maybe String -> Html Msg -> Html Msg
inputFieldView lab maybeDesc inp =
  div [ class "form-field" ]
    [ label []
      [ h6 [] [ text lab ]
      , case maybeDesc of
        Just desc -> Html.p [] [ text desc ]
        Nothing -> text ""
      , inp
      ]
    ]

dmpEditorView : Dmp -> Bool -> EditorMode -> User.LoginSession -> OrgLookup -> Html Msg
dmpEditorView dmp d mode session orgs =
  let
    orgToOption org = option [ value org, selected <| dmp.dmpOrgId == org ] [ text org ]
  in div [ class "dmp-editor" ]
    [ h2 [] [ text "DMP - Lomake" ]
    , displayFieldView "Luotu: " (Maybe.map showUtcTime dmp.dmpCreated)
    , displayFieldView "Muokattu: " (Maybe.map showUtcTime dmp.dmpModified)
    , div [ class "form-field" ] <| case mode of
      Edit _ -> [ label [] [ text <| "Organisaatio: " ++ showOrgName dmp orgs ] ]
      New ->
        case session of
          User.LoggedIn personToken person ->
            if Array.length person.organisation > 1 then
              [ label [ for "dmp-editor-org" ] [ text "Organisaatio*: " ]
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
            else [ label [] [ text <| "Organisaatio: " ++ Maybe.withDefault "-" (Array.get 0 person.organisation) ] ]
          _ -> [ text "Kirjaudu sisään, jotta voit käsitellä DMP:itä." ]
    , inputFieldView "Otsikko*: " Nothing
      <| input
        [ value dmp.dmpTitle
        , disabled d
        , onInput <| OnModifyDmp << ModifyDmpTitle
        ]
        []
    , inputFieldView "Kuvaus: " Nothing
        <| textarea
          [ value <| withDefault "" dmp.dmpDescription
          , disabled d
          , onInput <| OnModifyDmp << ModifyDmpDescription << parseMaybe
          , class "d-block"
          ]
          []
    , inputFieldView "Kieli: " Nothing
        <| languageSelect dmp.dmpLanguage d <| OnModifyDmp << ModifyDmpLanguage
    , inputFieldView "Seuraava tarkastuspäivä: " (Just "Ilmoita tähän päivämäärä, jolloin aineistonhallintasuunnitelma tarkastetaan seuraavan kerran.")
        <| input
          [ type_ "date"
          , value <| withDefault "" (Maybe.map unwrapDay dmp.dmpNextReviewDmp)
          , disabled d
          , onInput <| OnModifyDmp << ModifyDmpNextReviewDmp << Maybe.map Day << parseMaybe
          ]
          []
    , inputFieldView "Tyyppi: " (Just "Ilmoita 'Priodiversity LIFE', mikäli aineistonhallintasuunnitelma liittyy Priodiversity LIFE -hankkeeseen.")
        <| dmpTypeSelect dmp.dmpTypeDmp d <| OnModifyDmp << ModifyDmpTypeDmp
    , section [] [ dmpIdEditorView dmp.dmpDmpId d ]
    , section [] [ contactEditorView dmp.dmpContact d ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\idx elem -> contributorEditorView idx elem d) dmp.dmpContributors
      , button
        [ onClick <| OnModifyDmp AddDmpContributor
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää osallistuja" ]
      ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\idx elem -> projectEditorView idx elem d) dmp.dmpProjects
      , button
        [ onClick <| OnModifyDmp AddDmpProject
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää projekti" ]
        ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\idx elem -> datasetEditorView idx elem d) dmp.dmpDatasets
      , button
        [ onClick <| OnModifyDmp AddDmpDataset
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää aineisto" ]
        ]
    , section []
      [ div []
        <| Array.toList <| Array.indexedMap (\idx elem -> ethicalIssueEditorView idx elem d) dmp.dmpEthicalIssues
      , button
        [ onClick <| OnModifyDmp AddDmpEthicalIssue
        , disabled d
        , class "btn"
        ]
        [ text "+ Lisää 'eettiset haasteet' -osio" ]
        ]
    , hr [] []
    ]

editorFormView : Model -> OrgLookup -> Html Msg
editorFormView model orgs = 
  div [ class "dmp-editor-wrapper" ]
  [ div []
      [ dmpEditorView model.dmp (model.status == Submitting) model.mode model.session orgs
      , button
        [ onClick OnSubmit
        , disabled (model.status == Submitting)
        , class "btn btn-primary"
        ]
        [ text "Tallenna" ]
      ]
  , case model.status of
    SubmitError e -> div [ class "dmp-editor-status" ] [ errorResponseView e ]
    NotLoggedInError -> div [ class "dmp-editor-status" ] [ text <| "Virhe DMP:tä tallennettaessa: Kirjautuminen puuttuu!" ]
    _ -> text ""
  ]

view : Model -> OrgLookup -> Html Msg
view = editorFormView

