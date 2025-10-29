module Models exposing (..)

import Array

type Day = Day String

unwrapDay : Day -> String
unwrapDay (Day s) = s

type UTCTime = UTCTime String

unwrapUtcTime : UTCTime -> String
unwrapUtcTime (UTCTime s) = s

type DataAccessType 
  = DataAccessTypeOpen 
  | DataAccessTypeShared
  | DataAccessTypeClosed
  | DataAccessTypeClassified
  | DataAccessTypeEmbargoed

type DmpType
  = DmpTypeStudent
  | DmpTypeAcademic
  | DmpTypeNational
  | DmpTypeInternational
  | DmpTypeOrganizational
  | DmpTypePriodiversityLife

type DocumentIdType
  = DocumentIdTypeHandle
  | DocumentIdTypeDoi
  | DocumentIdTypeArk
  | DocumentIdTypeUrl
  | DocumentIdTypeOther
  | DocumentIdTypeNone

type EthicalIssuesType
  = EthicalIssuesTypeYes
  | EthicalIssuesTypeNo
  | EthicalIssuesTypeUnknown

type LanguageType
  = LanguageTypeFi
  | LanguageTypeSv
  | LanguageTypeEn
  | LanguageTypeOther

type MetadataIdType
  = MetadataIdTypeUrl
  | MetadataIdTypeOther
  | MetadataIdTypeNone

type PersonIdType
  = PersonIdTypeOrcid
  | PersonIdTypeIsni
  | PersonIdTypeOpenid
  | PersonIdTypeOther
  | PersonIdTypeNone

type PersonalDataType
  = PersonalDataTypeYes
  | PersonalDataTypeNo
  | PersonalDataTypeUnknown

type RoleType
  = RoleTypeProjectDataController
  | RoleTypeDataOwner
  | RoleTypeOrganizationDataController
  | RoleTypeDatasetAuthor
  | RoleTypeOther

type SensitiveDataType
  = SensitiveDataTypeYes
  | SensitiveDataTypeNo
  | SensitiveDataTypeUnknown

type alias Contact =
  { contactMbox: String
  , contactName: String
  , contactOrganization: Maybe String
  , contactContactId: ContactId
  }

type alias ContactId =
  { contactIdIdentifier: Maybe String
  , contactIdType: PersonIdType
  }

type alias Contributor =
  { contributorMbox: Maybe String
  , contributorName: String
  , contributorOrganization: Maybe String
  , contributorRole: RoleType
  , contributorContributorId: ContributorId
  }

type alias ContributorId =
  { contributorIdIdentifier: Maybe String
  , contributorIdType: PersonIdType
  }

type alias DataLifeCycle =
  { dataLifeCycleArchivingServicesData: Bool
  , dataLifeCycleBackupData: String
  , dataLifeCycleDeletionWhenData: Maybe Day
  , dataLifeCycleUpdateFrequency: String
  }

type alias Dataset =
  { datasetDataQualityAssurance: Maybe String
  , datasetDataSharingIssues: Maybe String
  , datasetDescription: Maybe String
  , datasetIssued: Maybe Day
  , datasetKeywords: Maybe (Array.Array String)
  , datasetLanguage: LanguageType
  , datasetPersonalData: PersonalDataType
  , datasetSensitiveData: SensitiveDataType
  , datasetReuseDataset: Maybe Bool
  , datasetTitle: String
  , datasetType: Maybe String
  , datasetVocabulary: Maybe (Array.Array String)
  , datasetDatasetId: DatasetId
  , datasetDistributions: Array.Array Distribution
  , datasetMetadata: Array.Array Metadata
  , datasetSecurityAndPrivacy: Array.Array SecurityAndPrivacy
  , datasetDataLifeCycle: Maybe DataLifeCycle
  }

type alias DatasetId =
  { datasetIdIdentifier: Maybe String
  , datasetIdType: DocumentIdType
  }

type alias Distribution =
  { distributionAccessUrl: Maybe String
  , distributionDataAccess: Maybe DataAccessType
  , distributionDescription: Maybe String
  , distributionDownloadUri: Maybe String
  , distributionFormat: Maybe String
  , distributionTitle: String
  , distributionLicenses: Array.Array License
  }

type alias Dmp =
  { dmpId: Maybe Int
  , dmpCreated: Maybe UTCTime
  , dmpDescription: Maybe String
  , dmpLanguage: LanguageType
  , dmpModified: Maybe UTCTime
  , dmpNextReviewDmp: Maybe Day
  , dmpOrgId: String
  , dmpTitle: String
  , dmpTypeDmp: DmpType
  , dmpContact: Contact
  , dmpDmpId: DmpId
  , dmpContributors: Array.Array Contributor
  , dmpDatasets: Array.Array Dataset
  , dmpEthicalIssues: Array.Array EthicalIssue
  , dmpProjects: Array.Array Project
  }

type alias DmpId =
  { dmpIdIdentifier: Maybe String
  , dmpIdType: DocumentIdType
  }

type alias EthicalIssue =
  { ethicalIssueDescription: Maybe String
  , ethicalIssueExist: EthicalIssuesType
  , ethicalIssueReport: Maybe String
  }

type alias License =
  { licenseRef: String
  , licenseStartDate: Day
  }

type alias Metadata =
  { metadataLanguage: LanguageType
  , metadataOpen: Maybe Bool
  , metadataLocation: Maybe String
  , metadataStandards: Maybe (Array.Array String)
  , metadataMetadataId: MetadataId
  }

type alias MetadataId =
  { metadataIdIdentifier: Maybe String
  , metadataIdType: MetadataIdType
  }

type alias Project =
  { projectDescription: String
  , projectEndDate: Maybe Day
  , projectStartDate: Day
  , projectTitle: String
  }

type alias SecurityAndPrivacy =
  { securityDescription: String
  , securityTitle: String
  }

