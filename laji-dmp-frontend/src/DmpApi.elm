module DmpApi exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Array
import Http
import Config exposing (config)
import Json.Encode as E
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Url.Builder as UB
import Models exposing (..)

dayDecoder : D.Decoder Day
dayDecoder = D.map (\str -> Day str) D.string

encodeDay : Day -> E.Value
encodeDay (Day d) = E.string d

utcTimeDecoder : D.Decoder UTCTime
utcTimeDecoder = D.map (\str -> UTCTime str) D.string

encodeUtcTime : UTCTime -> E.Value
encodeUtcTime (UTCTime d) = E.string d

encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe f m = case m of
  Just val -> f val
  Nothing -> E.null

dataAccessTypeDecoder : Json.Decode.Decoder DataAccessType
dataAccessTypeDecoder = Json.Decode.map (
  \str -> case str of
    "DataAccessTypeOpen" -> DataAccessTypeOpen
    "DataAccessTypeShared" -> DataAccessTypeShared
    _ -> DataAccessTypeClosed
  ) D.string

encodeDataAccessType : DataAccessType -> E.Value
encodeDataAccessType t = E.string
  <| case t of
    DataAccessTypeOpen -> "DataAccessTypeOpen"
    DataAccessTypeShared -> "DataAccessTypeShared"
    DataAccessTypeClosed -> "DataAccessTypeClosed"

deletionDataTypeDecoder : Json.Decode.Decoder DeletionDataType
deletionDataTypeDecoder = Json.Decode.map
  (\str -> case str of
    "DeletionDataTypeYes" -> DeletionDataTypeYes
    "DeletionDataTypeNo" -> DeletionDataTypeNo
    _ -> DeletionDataTypeUnknown
  ) D.string

encodeDeletionDataType : DeletionDataType -> E.Value
encodeDeletionDataType t = E.string <| case t of
  DeletionDataTypeYes -> "DeletionDataTypeYes"
  DeletionDataTypeNo -> "DeletionDataTypeNo"
  DeletionDataTypeUnknown -> "DeletionDataTypeUnknown"

dmpTypeDecoder : Json.Decode.Decoder DmpType
dmpTypeDecoder = Json.Decode.map
  (\str -> case str of
    "DmpTypeStudent" -> DmpTypeStudent
    "DmpTypeAcademic" -> DmpTypeAcademic
    "DmpTypeNational" -> DmpTypeNational
    "DmpTypeInternational" -> DmpTypeInternational
    _ -> DmpTypeOrganizational
  ) D.string

encodeDmpType : DmpType -> E.Value
encodeDmpType t = E.string <| case t of
  DmpTypeStudent -> "DmpTypeStudent"
  DmpTypeAcademic -> "DmpTypeAcademic"
  DmpTypeNational -> "DmpTypeNational"
  DmpTypeInternational -> "DmpTypeInternational"
  DmpTypeOrganizational -> "DmpTypeOrganizational"

documentIdTypeDecoder : Json.Decode.Decoder DocumentIdType
documentIdTypeDecoder = Json.Decode.map
  (\str -> case str of
    "DocumentIdTypeHandle" -> DocumentIdTypeHandle
    "DocumentIdTypeDoi" -> DocumentIdTypeDoi
    "DocumentIdTypeArk" -> DocumentIdTypeArk
    "DocumentIdTypeUrl" -> DocumentIdTypeUrl
    "DocumentIdTypeOther" -> DocumentIdTypeOther
    _ -> DocumentIdTypeNone
  ) D.string

encodeDocumentIdType : DocumentIdType -> E.Value
encodeDocumentIdType t = E.string <| case t of
  DocumentIdTypeHandle -> "DocumentIdTypeHandle"
  DocumentIdTypeDoi -> "DocumentIdTypeDoi"
  DocumentIdTypeArk -> "DocumentIdTypeArk"
  DocumentIdTypeUrl -> "DocumentIdTypeUrl"
  DocumentIdTypeOther -> "DocumentIdTypeOther"
  DocumentIdTypeNone -> "DocumentIdTypeNone"

ethicalIssuesTypeDecoder : Json.Decode.Decoder EthicalIssuesType
ethicalIssuesTypeDecoder = Json.Decode.map
  (\str -> case str of
    "EthicalIssuesTypeYes" -> EthicalIssuesTypeYes
    "EthicalIssuesTypeNo" -> EthicalIssuesTypeNo
    _ -> EthicalIssuesTypeUnknown
  ) D.string

encodeEthicalIssuesType : EthicalIssuesType -> E.Value
encodeEthicalIssuesType t = E.string <| case t of
  EthicalIssuesTypeYes -> "EthicalIssuesTypeYes"
  EthicalIssuesTypeNo -> "EthicalIssuesTypeNo"
  EthicalIssuesTypeUnknown -> "EthicalIssuesTypeUnknown"

languageTypeDecoder : Json.Decode.Decoder LanguageType
languageTypeDecoder = Json.Decode.map
  (\str -> case str of
    "LanguageTypeFi" -> LanguageTypeFi
    "LanguageTypeSv" -> LanguageTypeSv
    _ -> LanguageTypeEn
  ) D.string

encodeLanguageType : LanguageType -> E.Value
encodeLanguageType t = E.string <| case t of
  LanguageTypeFi -> "LanguageTypeFi"
  LanguageTypeSv -> "LanguageTypeSv"
  LanguageTypeEn -> "LanguageTypeEn"

metadataIdTypeDecoder : Json.Decode.Decoder MetadataIdType
metadataIdTypeDecoder = Json.Decode.map
  (\str -> case str of
    "MetadataIdTypeUrl" -> MetadataIdTypeUrl
    "MetadataIdTypeOther" -> MetadataIdTypeOther
    _ -> MetadataIdTypeNone
  ) D.string

encodeMetadataIdType : MetadataIdType -> E.Value
encodeMetadataIdType t = E.string <| case t of
  MetadataIdTypeUrl -> "MetadataIdTypeUrl"
  MetadataIdTypeOther -> "MetadataIdTypeOther"
  MetadataIdTypeNone -> "MetadataIdTypeNone"

personIdTypeDecoder : Json.Decode.Decoder PersonIdType
personIdTypeDecoder = Json.Decode.map
  (\str -> case str of
    "PersonIdTypeOrcid" -> PersonIdTypeOrcid
    "PersonIdTypeIsni" -> PersonIdTypeIsni
    "PersonIdTypeOpenid" -> PersonIdTypeOpenid
    "PersonIdTypeOther" -> PersonIdTypeOther
    _ -> PersonIdTypeNone
  ) D.string

encodePersonIdType : PersonIdType -> E.Value
encodePersonIdType t = E.string <| case t of
  PersonIdTypeOrcid -> "PersonIdTypeOrcid"
  PersonIdTypeIsni -> "PersonIdTypeIsni"
  PersonIdTypeOpenid -> "PersonIdTypeOpenid"
  PersonIdTypeOther -> "PersonIdTypeOther"
  PersonIdTypeNone -> "PersonIdTypeNone"

personalDataTypeDecoder : Json.Decode.Decoder PersonalDataType
personalDataTypeDecoder = Json.Decode.map
  (\str -> case str of
    "PersonalDataTypeYes" -> PersonalDataTypeYes
    "PersonalDataTypeNo" -> PersonalDataTypeNo
    _ -> PersonalDataTypeUnknown
  ) D.string

encodePersonalDataType : PersonalDataType -> E.Value
encodePersonalDataType t = E.string <| case t of
  PersonalDataTypeYes -> "PersonalDataTypeYes"
  PersonalDataTypeNo -> "PersonalDataTypeNo"
  PersonalDataTypeUnknown -> "PersonalDataTypeUnknown"

roleTypeDecoder : Json.Decode.Decoder RoleType
roleTypeDecoder = Json.Decode.map
  (\str -> case str of
    "RoleTypeWorkPackageLeader" -> RoleTypeWorkPackageLeader
    "RoleTypeDataController" -> RoleTypeDataController
    "RoleTypePrincipleInvestigator" -> RoleTypePrincipleInvestigator
    "RoleTypeAuthorOfDataSet" -> RoleTypeAuthorOfDataSet
    _ -> RoleTypeOther
  ) D.string

encodeRoleType : RoleType -> E.Value
encodeRoleType t = E.string <| case t of
  RoleTypeWorkPackageLeader -> "RoleTypeWorkPackageLeader"
  RoleTypeDataController -> "RoleTypeDataController"
  RoleTypePrincipleInvestigator -> "RoleTypePrincipleInvestigator"
  RoleTypeAuthorOfDataSet -> "RoleTypeAuthorOfDataSet"
  RoleTypeOther -> "RoleTypeOther"

sensitiveDataTypeDecoder : Json.Decode.Decoder SensitiveDataType
sensitiveDataTypeDecoder = Json.Decode.map
  (\str -> case str of
    "SensitiveDataTypeYes" -> SensitiveDataTypeYes
    "SensitiveDataTypeNo" -> SensitiveDataTypeNo
    _ -> SensitiveDataTypeUnknown
  ) D.string

encodeSensitiveDataType : SensitiveDataType -> E.Value
encodeSensitiveDataType t = E.string <| case t of
  SensitiveDataTypeYes -> "SensitiveDataTypeYes"
  SensitiveDataTypeNo -> "SensitiveDataTypeNo"
  SensitiveDataTypeUnknown -> "SensitiveDataTypeUnknown"

contactDecoder : Json.Decode.Decoder Contact
contactDecoder = Json.Decode.succeed Contact
  |> DP.required "contactMbox" D.string
  |> DP.required "contactName" D.string
  |> DP.optional "contactOrganization" (D.nullable D.string) Nothing
  |> DP.required "contactContactId" contactIdDecoder

encodeContact : Contact -> E.Value
encodeContact t = E.object
  [ ( "contactMbox", E.string t.contactMbox)
  , ( "contactName", E.string t.contactName)
  , ( "contactOrganization", encodeMaybe E.string t.contactOrganization)
  , ( "contactContactId", encodeContactId t.contactContactId)
  ]

contactIdDecoder : Json.Decode.Decoder ContactId
contactIdDecoder = Json.Decode.succeed ContactId
  |> DP.optional "contactIdIdentifier" (D.nullable D.string) Nothing
  |> DP.required "contactIdType" personIdTypeDecoder

encodeContactId : ContactId -> E.Value
encodeContactId t = E.object
  [ ( "contactIdIdentifier", encodeMaybe E.string t.contactIdIdentifier)
  , ( "contactIdType", encodePersonIdType t.contactIdType)
  ]

contributorDecoder : Json.Decode.Decoder Contributor
contributorDecoder = Json.Decode.succeed Contributor
  |> DP.optional "contributorMbox" (D.nullable D.string) Nothing
  |> DP.required "contributorName" D.string
  |> DP.optional "contributorOrganization" (D.nullable D.string) Nothing
  |> DP.required "contributorRole" roleTypeDecoder
  |> DP.required "contributorContributorId" contributorIdDecoder

encodeContributor : Contributor -> E.Value
encodeContributor t = E.object
  [ ( "contributorMbox", encodeMaybe E.string t.contributorMbox)
  , ( "contributorName", E.string t.contributorName)
  , ( "contributorOrganization", encodeMaybe E.string t.contributorOrganization)
  , ( "contributorRole", encodeRoleType t.contributorRole)
  , ( "contributorContributorId", encodeContributorId t.contributorContributorId)
  ]

contributorIdDecoder : Json.Decode.Decoder ContributorId
contributorIdDecoder = Json.Decode.succeed ContributorId
  |> DP.optional "contributorIdIdentifier" (D.nullable D.string) Nothing
  |> DP.required "contributorIdType" personIdTypeDecoder

encodeContributorId : ContributorId -> E.Value
encodeContributorId t = E.object
  [ ( "contributorIdIdentifier", encodeMaybe E.string t.contributorIdIdentifier)
  , ( "contributorIdType", encodePersonIdType t.contributorIdType)
  ]

dataLifeCycleDecoder : Json.Decode.Decoder DataLifeCycle
dataLifeCycleDecoder = Json.Decode.succeed DataLifeCycle
  |> DP.required "dataLifeCycleArchivingServicesData" D.bool
  |> DP.required "dataLifeCycleBackupData" D.string
  |> DP.required "dataLifeCycleDeletionData" deletionDataTypeDecoder
  |> DP.optional "dataLifeCycleDeletionWhenData" (D.nullable dayDecoder) Nothing

encodeDataLifeCycle : DataLifeCycle -> E.Value
encodeDataLifeCycle t = E.object
  [ ( "dataLifeCycleArchivingServicesData", E.bool t.dataLifeCycleArchivingServicesData)
  , ( "dataLifeCycleBackupData", E.string t.dataLifeCycleBackupData)
  , ( "dataLifeCycleDeletionData", encodeDeletionDataType t.dataLifeCycleDeletionData)
  , ( "dataLifeCycleDeletionWhenData", encodeMaybe encodeDay t.dataLifeCycleDeletionWhenData)
  ]

datasetDecoder : Json.Decode.Decoder Dataset
datasetDecoder = Json.Decode.succeed Dataset
  |> DP.optional "datasetDataQualityAssurance" (D.nullable D.string) Nothing
  |> DP.optional "datasetDataSharingIssues" (D.nullable D.string) Nothing
  |> DP.optional "datasetDescription" (D.nullable D.string) Nothing
  |> DP.optional "datasetIssued" (D.nullable dayDecoder) Nothing
  |> DP.optional "datasetKeywords" (D.nullable (D.array D.string)) Nothing
  |> DP.optional "datasetLanguage" (D.nullable languageTypeDecoder) Nothing
  |> DP.required "datasetPersonalData" personalDataTypeDecoder
  |> DP.required "datasetSensitiveData" sensitiveDataTypeDecoder
  |> DP.optional "datasetReuseDataset" (D.nullable D.bool) Nothing
  |> DP.required "datasetTitle" D.string
  |> DP.optional "datasetType" (D.nullable D.string) Nothing
  |> DP.required "datasetDatasetId" datasetIdDecoder
  |> DP.required "datasetDistributions" (D.array distributionDecoder)
  |> DP.required "datasetMetadata" (D.array metadataDecoder)
  |> DP.required "datasetRightsRelatedToData" (D.array rightsDecoder)
  |> DP.required "datasetSecurityAndPrivacy" (D.array securityDecoder)

encodeDataset : Dataset -> E.Value
encodeDataset t = E.object
  [ ( "datasetDataQualityAssurance", encodeMaybe E.string t.datasetDataQualityAssurance)
  , ( "datasetDataSharingIssues", encodeMaybe E.string t.datasetDataSharingIssues)
  , ( "datasetDescription", encodeMaybe E.string t.datasetDescription)
  , ( "datasetIssued", encodeMaybe encodeDay t.datasetIssued)
  , ( "datasetKeywords", encodeMaybe (E.array E.string) t.datasetKeywords)
  , ( "datasetLanguage", encodeMaybe encodeLanguageType t.datasetLanguage)
  , ( "datasetPersonalData", encodePersonalDataType t.datasetPersonalData)
  , ( "datasetSensitiveData", encodeSensitiveDataType t.datasetSensitiveData)
  , ( "datasetReuseDataset", encodeMaybe E.bool t.datasetReuseDataset)
  , ( "datasetTitle", E.string t.datasetTitle)
  , ( "datasetType", encodeMaybe E.string t.datasetType)
  , ( "datasetDatasetId", encodeDatasetId t.datasetDatasetId)
  , ( "datasetDistributions", E.array encodeDistribution t.datasetDistributions)
  , ( "datasetMetadata", E.array encodeMetadata t.datasetMetadata)
  , ( "datasetRightsRelatedToData", E.array encodeRights t.datasetRightsRelatedToData)
  , ( "datasetSecurityAndPrivacy", E.array encodeSecurity t.datasetSecurityAndPrivacy)
  ]

datasetIdDecoder : Json.Decode.Decoder DatasetId
datasetIdDecoder = Json.Decode.succeed DatasetId
  |> DP.optional "datasetIdIdentifier" (D.nullable D.string) Nothing
  |> DP.required "datasetIdType" documentIdTypeDecoder

encodeDatasetId : DatasetId -> E.Value
encodeDatasetId t = E.object
  [ ( "datasetIdIdentifier", encodeMaybe E.string t.datasetIdIdentifier)
  , ( "datasetIdType", encodeDocumentIdType t.datasetIdType)
  ]

distributionDecoder : Json.Decode.Decoder Distribution
distributionDecoder = Json.Decode.succeed Distribution
  |> DP.optional "distributionAccessUrl" (D.nullable D.string) Nothing
  |> DP.optional "distributionDataAccess" (D.nullable dataAccessTypeDecoder) Nothing
  |> DP.optional "distributionDescription" (D.nullable D.string) Nothing
  |> DP.optional "distributionDownloadUri" (D.nullable D.string) Nothing
  |> DP.optional "distributionFormat" (D.nullable D.string) Nothing
  |> DP.required "distributionTitle" D.string
  |> DP.required "distributionLicenses" (D.array licenseDecoder)

encodeDistribution : Distribution -> E.Value
encodeDistribution t = E.object
  [ ( "distributionAccessUrl", encodeMaybe E.string t.distributionAccessUrl)
  , ( "distributionDataAccess", encodeMaybe encodeDataAccessType t.distributionDataAccess)
  , ( "distributionDescription", encodeMaybe E.string t.distributionDescription)
  , ( "distributionDownloadUri", encodeMaybe E.string t.distributionDownloadUri)
  , ( "distributionFormat", encodeMaybe E.string t.distributionFormat)
  , ( "distributionTitle", E.string t.distributionTitle)
  , ( "distributionLicenses", E.array encodeLicense t.distributionLicenses)
  ]

dmpDecoder : Json.Decode.Decoder Dmp
dmpDecoder = Json.Decode.succeed Dmp
  |> DP.optional "dmpId" (D.nullable D.int) Nothing
  |> DP.optional "dmpCreated" (D.nullable utcTimeDecoder) Nothing
  |> DP.optional "dmpDescription" (D.nullable D.string) Nothing
  |> DP.required "dmpLanguage" languageTypeDecoder
  |> DP.optional "dmpModified" (D.nullable utcTimeDecoder) Nothing
  |> DP.optional "dmpNextReviewDmp" (D.nullable dayDecoder) Nothing
  |> DP.required "dmpOrgId" D.string
  |> DP.required "dmpTitle" D.string
  |> DP.required "dmpTypeDmp" dmpTypeDecoder
  |> DP.required "dmpContact" contactDecoder
  |> DP.required "dmpDmpId" dmpIdDecoder
  |> DP.required "dmpContributors" (D.array contributorDecoder)
  |> DP.required "dmpDataLifeCycles" (D.array dataLifeCycleDecoder)
  |> DP.required "dmpDatasets" (D.array datasetDecoder)
  |> DP.required "dmpEthicalIssues" (D.array ethicalIssueDecoder)
  |> DP.required "dmpProjects" (D.array projectDecoder)

encodeDmp : Dmp -> E.Value
encodeDmp t = E.object
  [ ( "dmpId", encodeMaybe E.int t.dmpId)
  , ( "dmpCreated", encodeMaybe encodeUtcTime t.dmpCreated)
  , ( "dmpDescription", encodeMaybe E.string t.dmpDescription)
  , ( "dmpLanguage", encodeLanguageType t.dmpLanguage)
  , ( "dmpModified", encodeMaybe encodeUtcTime t.dmpModified)
  , ( "dmpNextReviewDmp", encodeMaybe encodeDay t.dmpNextReviewDmp)
  , ( "dmpOrgId", E.string t.dmpOrgId)
  , ( "dmpTitle", E.string t.dmpTitle)
  , ( "dmpTypeDmp", encodeDmpType t.dmpTypeDmp)
  , ( "dmpContact", encodeContact t.dmpContact)
  , ( "dmpDmpId", encodeDmpId t.dmpDmpId)
  , ( "dmpContributors", E.array encodeContributor t.dmpContributors)
  , ( "dmpDataLifeCycles", E.array encodeDataLifeCycle t.dmpDataLifeCycles)
  , ( "dmpDatasets", E.array encodeDataset t.dmpDatasets)
  , ( "dmpEthicalIssues", E.array encodeEthicalIssue t.dmpEthicalIssues)
  , ( "dmpProjects", E.array encodeProject t.dmpProjects)
  ]

dmpIdDecoder : Json.Decode.Decoder DmpId
dmpIdDecoder = Json.Decode.succeed DmpId
  |> DP.optional "dmpIdIdentifier" (D.nullable D.string) Nothing
  |> DP.required "dmpIdType" documentIdTypeDecoder

encodeDmpId : DmpId -> E.Value
encodeDmpId t = E.object
  [ ( "dmpIdIdentifier", encodeMaybe E.string t.dmpIdIdentifier)
  , ( "dmpIdType", encodeDocumentIdType t.dmpIdType)
  ]

ethicalIssueDecoder : Json.Decode.Decoder EthicalIssue
ethicalIssueDecoder = Json.Decode.succeed EthicalIssue
  |> DP.optional "ethicalIssueDescription" (D.nullable D.string) Nothing
  |> DP.required "ethicalIssueExist" ethicalIssuesTypeDecoder
  |> DP.optional "ethicalIssueReport" (D.nullable D.string) Nothing

encodeEthicalIssue : EthicalIssue -> E.Value
encodeEthicalIssue t = E.object
  [ ( "ethicalIssueDescription", encodeMaybe E.string t.ethicalIssueDescription)
  , ( "ethicalIssueExist", encodeEthicalIssuesType t.ethicalIssueExist)
  , ( "ethicalIssueReport", encodeMaybe E.string t.ethicalIssueReport)
  ]

licenseDecoder : Json.Decode.Decoder License
licenseDecoder = Json.Decode.succeed License
  |> DP.required "licenseRef" D.string
  |> DP.required "licenseStartDate" dayDecoder

encodeLicense : License -> E.Value
encodeLicense t = E.object
  [ ( "licenseRef", E.string t.licenseRef)
  , ( "licenseStartDate", encodeDay t.licenseStartDate)
  ]

metadataDecoder : Json.Decode.Decoder Metadata
metadataDecoder = Json.Decode.succeed Metadata
  |> DP.optional "metadataAccessDocumentation" (D.nullable D.bool) Nothing
  |> DP.optional "metadataDataModel" (D.nullable D.string) Nothing
  |> DP.optional "metadataDescription" (D.nullable D.string) Nothing
  |> DP.required "metadataLanguage" languageTypeDecoder
  |> DP.optional "metadataLocationDocumentation" (D.nullable D.string) Nothing
  |> DP.optional "metadataOpen" (D.nullable D.bool) Nothing
  |> DP.optional "metadataLocation" (D.nullable D.string) Nothing
  |> DP.optional "metadataSchema" (D.nullable D.bool) Nothing
  |> DP.required "metadataMetadataId" metadataIdDecoder

encodeMetadata : Metadata -> E.Value
encodeMetadata t = E.object
  [ ( "metadataAccessDocumentation", encodeMaybe E.bool t.metadataAccessDocumentation)
  , ( "metadataDataModel", encodeMaybe E.string t.metadataDataModel)
  , ( "metadataDescription", encodeMaybe E.string t.metadataDescription)
  , ( "metadataLanguage", encodeLanguageType t.metadataLanguage)
  , ( "metadataLocationDocumentation", encodeMaybe E.string t.metadataLocationDocumentation)
  , ( "metadataOpen", encodeMaybe E.bool t.metadataOpen)
  , ( "metadataLocation", encodeMaybe E.string t.metadataLocation)
  , ( "metadataSchema", encodeMaybe E.bool t.metadataSchema)
  , ( "metadataMetadataId", encodeMetadataId t.metadataMetadataId)
  ]

metadataIdDecoder : Json.Decode.Decoder MetadataId
metadataIdDecoder = Json.Decode.succeed MetadataId
  |> DP.optional "metadataIdIdentifier" (D.nullable D.string) Nothing
  |> DP.required "metadataIdType" metadataIdTypeDecoder

encodeMetadataId : MetadataId -> E.Value
encodeMetadataId t = E.object
  [ ( "metadataIdIdentifier", encodeMaybe E.string t.metadataIdIdentifier)
  , ( "metadataIdType", encodeMetadataIdType t.metadataIdType)
  ]

projectDecoder : Json.Decode.Decoder Project
projectDecoder = Json.Decode.succeed Project
  |> DP.required "projectDescription" D.string
  |> DP.optional "projectEndDate" (D.nullable dayDecoder) Nothing
  |> DP.required "projectStartDate" dayDecoder
  |> DP.required "projectTitle" D.string

encodeProject : Project -> E.Value
encodeProject t = E.object
  [ ( "projectDescription", E.string t.projectDescription)
  , ( "projectEndDate", encodeMaybe encodeDay t.projectEndDate)
  , ( "projectStartDate", encodeDay t.projectStartDate)
  , ( "projectTitle", E.string t.projectTitle)
  ]

rightsDecoder : Json.Decode.Decoder RightsRelatedToData
rightsDecoder = Json.Decode.succeed RightsRelatedToData
  |> DP.optional "rightsOwnershipDataRight" (D.nullable D.string) Nothing

encodeRights : RightsRelatedToData -> E.Value
encodeRights t = E.object
  [ ( "rightsOwnershipDataRight", encodeMaybe E.string t.rightsOwnershipDataRight)
  ]

securityDecoder : Json.Decode.Decoder SecurityAndPrivacy
securityDecoder = Json.Decode.succeed SecurityAndPrivacy
  |> DP.required "securityDescription" D.string
  |> DP.required "securityTitle" D.string

encodeSecurity : SecurityAndPrivacy -> E.Value
encodeSecurity t = E.object
  [ ( "securityDescription", E.string t.securityDescription)
  , ( "securityTitle", E.string t.securityTitle)
  ]

-- Queries

getDmpList : (Result Http.Error (Array.Array Dmp) -> msg) -> Cmd msg
getDmpList msg =
  Http.get
    { url = UB.crossOrigin config.dmpApiUrl ["dmp"] []
    , expect = Http.expectJson msg (D.array dmpDecoder)
    }

getDmp : Int -> (Result Http.Error Dmp -> msg) -> Cmd msg
getDmp id msg =
  Http.get
    { url = UB.crossOrigin config.dmpApiUrl ["dmp", String.fromInt id] []
    , expect = Http.expectJson msg dmpDecoder
    }

newDmp : Dmp -> String -> (Result Http.Error String -> msg) -> Cmd msg
newDmp dmp personToken msg =
  Http.request
    { method = "POST"
    , headers = []
    , url = UB.crossOrigin config.dmpApiUrl ["dmp"] [ UB.string "personToken" personToken ]
    , body = Http.jsonBody <| encodeDmp dmp
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }

editDmp : String -> Dmp -> String -> (Result Http.Error String -> msg) -> Cmd msg
editDmp id dmp personToken msg =
  Http.request
    { method = "PUT"
    , headers = []
    , url = UB.crossOrigin config.dmpApiUrl ["dmp", id] [ UB.string "personToken" personToken ]
    , body = Http.jsonBody <| encodeDmp dmp
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }

deleteDmp : String -> String -> (Result Http.Error String -> msg) -> Cmd msg
deleteDmp id personToken msg =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = UB.crossOrigin config.dmpApiUrl ["dmp", id] [ UB.string "personToken" personToken ]
    , body = Http.jsonBody <| E.null
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }

