{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Database.Models where

import Data.Aeson (ToJSON, FromJSON, toJSON, withText, Value(String))
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Prelude
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (Day, UTCTime)
import qualified Data.Text as T
import Data.Aeson.Types (parseJSON, prependFailure)
import Data.Swagger (ToSchema)
import Data.Int (Int64)
import Data.Swagger.Schema (ToSchema(declareNamedSchema))
import Data.Data (Proxy(..))

newtype NonEmptyText = NonEmptyText Text
  deriving (Show, Eq)

instance FromJSON NonEmptyText where
  parseJSON = withText "NonEmptyText" $ \t ->
    if T.null t
      then fail "EMPTY_TEXT"
      else pure (NonEmptyText t)

instance ToJSON NonEmptyText where
  toJSON (NonEmptyText t) = String t

instance ToSchema NonEmptyText where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToField NonEmptyText where
  toField (NonEmptyText t) = toField t

instance FromField NonEmptyText where
  fromField f mbs = do
    fromField f mbs

data DataAccessType
  = DataAccessTypeOpen
  | DataAccessTypeShared
  | DataAccessTypeClosed
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField DataAccessType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "open" -> return DataAccessTypeOpen
    Just "shared" -> return DataAccessTypeShared
    Just "closed" -> return DataAccessTypeClosed
    _ -> returnError ConversionFailed f $ "Invalid value for DataAccessType: " ++ show mbs

instance ToField DataAccessType where
  toField DataAccessTypeOpen = toField ("open" :: Text)
  toField DataAccessTypeShared = toField ("shared" :: Text)
  toField DataAccessTypeClosed = toField ("closed" :: Text)

data DeletionDataType
  = DeletionDataTypeYes
  | DeletionDataTypeNo
  | DeletionDataTypeUnknown
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField DeletionDataType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "yes" -> return DeletionDataTypeYes
    Just "no" -> return DeletionDataTypeNo
    Just "unknown" -> return DeletionDataTypeUnknown
    _ -> returnError ConversionFailed f $ "Invalid value for DeletionDataType: " ++ show mbs

instance ToField DeletionDataType where
  toField DeletionDataTypeYes = toField ("yes" :: Text)
  toField DeletionDataTypeNo = toField ("no" :: Text)
  toField DeletionDataTypeUnknown = toField ("unknown" :: Text)

data DmpType
  = DmpTypeStudent
  | DmpTypeAcademic
  | DmpTypeNational
  | DmpTypeInternational
  | DmpTypeOrganizational
  | DmpTypePriodiversityLife
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField DmpType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "student" -> return DmpTypeStudent
    Just "academic" -> return DmpTypeAcademic
    Just "national" -> return DmpTypeNational
    Just "international" -> return DmpTypeInternational
    Just "organizational" -> return DmpTypeOrganizational
    Just "priodiversity-life" -> return DmpTypePriodiversityLife
    _ -> returnError ConversionFailed f $ "Invalid value for DmpType: " ++ show mbs

instance ToField DmpType where
  toField DmpTypeStudent = toField ("student" :: Text)
  toField DmpTypeAcademic = toField ("academic" :: Text)
  toField DmpTypeNational = toField ("national" :: Text)
  toField DmpTypeInternational = toField ("international" :: Text)
  toField DmpTypeOrganizational = toField ("organizational" :: Text)
  toField DmpTypePriodiversityLife = toField ("priodiversity-life" :: Text)

data DocumentIdType
  = DocumentIdTypeHandle
  | DocumentIdTypeDoi
  | DocumentIdTypeArk
  | DocumentIdTypeUrl
  | DocumentIdTypeOther
  | DocumentIdTypeNone
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField DocumentIdType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "handle" -> return DocumentIdTypeHandle
    Just "doi" -> return DocumentIdTypeDoi
    Just "ark" -> return DocumentIdTypeArk
    Just "url" -> return DocumentIdTypeUrl
    Just "other" -> return DocumentIdTypeOther
    Just "none" -> return DocumentIdTypeNone
    _ -> returnError ConversionFailed f $ "Invalid value for DocumentIdType: " ++ show mbs

instance ToField DocumentIdType where
  toField DocumentIdTypeHandle = toField ("handle" :: Text)
  toField DocumentIdTypeDoi = toField ("doi" :: Text)
  toField DocumentIdTypeArk = toField ("ark" :: Text)
  toField DocumentIdTypeUrl = toField ("url" :: Text)
  toField DocumentIdTypeOther = toField ("other" :: Text)
  toField DocumentIdTypeNone = toField ("none" :: Text)

data EthicalIssuesType
  = EthicalIssuesTypeYes
  | EthicalIssuesTypeNo
  | EthicalIssuesTypeUnknown
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField EthicalIssuesType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "yes" -> return EthicalIssuesTypeYes
    Just "no" -> return EthicalIssuesTypeNo
    Just "unknown" -> return EthicalIssuesTypeUnknown
    _ -> returnError ConversionFailed f $ "Invalid value for EthicalIssuesType: " ++ show mbs

instance ToField EthicalIssuesType where
  toField EthicalIssuesTypeYes = toField ("yes" :: Text)
  toField EthicalIssuesTypeNo = toField ("no" :: Text)
  toField EthicalIssuesTypeUnknown = toField ("unknown" :: Text)

data LanguageType
  = LanguageTypeFi
  | LanguageTypeSv
  | LanguageTypeEn
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

languageTypeToText :: LanguageType -> Text
languageTypeToText LanguageTypeFi = "fi"
languageTypeToText LanguageTypeSv = "sv"
languageTypeToText LanguageTypeEn = "en"

languageTypeFromText :: Text -> Either String LanguageType
languageTypeFromText "fi" = Right LanguageTypeFi
languageTypeFromText "sv" = Right LanguageTypeSv
languageTypeFromText "en" = Right LanguageTypeEn
languageTypeFromText other = Left $ "Invalid LanguageType: " ++ T.unpack other

instance FromField LanguageType where
  fromField f mbs = case mbs of
    Just bs -> case languageTypeFromText (decodeUtf8 bs) of
      Right lt -> pure lt
      Left err -> returnError ConversionFailed f err
    Nothing -> returnError UnexpectedNull f "NULL for LanguageType"

instance ToField LanguageType where
  toField = toField . languageTypeToText

data MetadataIdType
  = MetadataIdTypeUrl
  | MetadataIdTypeOther
  | MetadataIdTypeNone
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField MetadataIdType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "url" -> return MetadataIdTypeUrl
    Just "other" -> return MetadataIdTypeOther
    Just "none" -> return MetadataIdTypeNone
    _ -> returnError ConversionFailed f $ "Invalid value for MetadataIdType: " ++ show mbs

instance ToField MetadataIdType where
  toField MetadataIdTypeUrl = toField ("url" :: Text)
  toField MetadataIdTypeOther = toField ("other" :: Text)
  toField MetadataIdTypeNone = toField ("none" :: Text)

data PersonIdType
  = PersonIdTypeOrcid
  | PersonIdTypeIsni
  | PersonIdTypeOpenid
  | PersonIdTypeOther
  | PersonIdTypeNone
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField PersonIdType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "orcid" -> return PersonIdTypeOrcid
    Just "isni" -> return PersonIdTypeIsni
    Just "openid" -> return PersonIdTypeOpenid
    Just "other" -> return PersonIdTypeOther
    Just "none" -> return PersonIdTypeNone
    _ -> returnError ConversionFailed f $ "Invalid value for PersonIdType: " ++ show mbs

instance ToField PersonIdType where
  toField PersonIdTypeOrcid = toField ("orcid" :: Text)
  toField PersonIdTypeIsni = toField ("isni" :: Text)
  toField PersonIdTypeOpenid = toField ("openid" :: Text)
  toField PersonIdTypeOther = toField ("other" :: Text)
  toField PersonIdTypeNone = toField ("none" :: Text)

data PersonalDataType
  = PersonalDataTypeYes
  | PersonalDataTypeNo
  | PersonalDataTypeUnknown
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField PersonalDataType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "yes" -> return PersonalDataTypeYes
    Just "no" -> return PersonalDataTypeNo
    Just "unknown" -> return PersonalDataTypeUnknown
    _ -> returnError ConversionFailed f $ "Invalid value for PersonalDataType: " ++ show mbs

instance ToField PersonalDataType where
  toField PersonalDataTypeYes = toField ("yes" :: Text)
  toField PersonalDataTypeNo = toField ("no" :: Text)
  toField PersonalDataTypeUnknown = toField ("unknown" :: Text)

data RoleType
  = RoleTypeWorkPackageLeader
  | RoleTypeDataController
  | RoleTypePrincipleInvestigator
  | RoleTypeAuthorOfDataSet
  | RoleTypeOther
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField RoleType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "work_package_leader" -> return RoleTypeWorkPackageLeader
    Just "data_controller" -> return RoleTypeDataController
    Just "principle_investigator" -> return RoleTypePrincipleInvestigator
    Just "author_of_dataset" -> return RoleTypeAuthorOfDataSet
    Just "other" -> return RoleTypeOther
    _ -> returnError ConversionFailed f $ "Invalid value for RoleType: " ++ show mbs

instance ToField RoleType where
  toField RoleTypeWorkPackageLeader = toField ("work_package_leader" :: Text)
  toField RoleTypeDataController = toField ("data_controller" :: Text)
  toField RoleTypePrincipleInvestigator = toField ("principle_investigator" :: Text)
  toField RoleTypeAuthorOfDataSet = toField ("author_of_dataset" :: Text)
  toField RoleTypeOther = toField ("other" :: Text)

data SensitiveDataType
  = SensitiveDataTypeYes
  | SensitiveDataTypeNo
  | SensitiveDataTypeUnknown
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField SensitiveDataType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "yes" -> return SensitiveDataTypeYes
    Just "no" -> return SensitiveDataTypeNo
    Just "unknown" -> return SensitiveDataTypeUnknown
    _ -> returnError ConversionFailed f $ "Invalid value for SensitiveDataType: " ++ show mbs

instance ToField SensitiveDataType where
  toField SensitiveDataTypeYes = toField ("yes" :: Text)
  toField SensitiveDataTypeNo = toField ("no" :: Text)
  toField SensitiveDataTypeUnknown = toField ("unknown" :: Text)

data Contact = Contact
  { contactMbox :: NonEmptyText
  , contactName :: NonEmptyText
  , contactOrganization :: Maybe Text
  , contactContactId :: ContactId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ContactId = ContactId
  { contactIdIdentifier :: Maybe Text
  , contactIdType :: PersonIdType
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Contributor = Contributor
  { contributorMbox :: Maybe Text
  , contributorName :: NonEmptyText
  , contributorOrganization :: Maybe Text
  , contributorRole :: RoleType
  , contributorContributorId :: ContributorId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ContributorId = ContributorId
  { contributorIdIdentifier :: Maybe Text
  , contributorIdType :: PersonIdType
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DataLifeCycle = DataLifeCycle
  { dataLifeCycleArchivingServicesData :: Bool
  , dataLifeCycleBackupData :: NonEmptyText
  , dataLifeCycleDeletionData :: DeletionDataType
  , dataLifeCycleDeletionWhenData :: Maybe Day
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Dataset = Dataset
  { datasetDataQualityAssurance :: Maybe Text
  , datasetDataSharingIssues :: Maybe Text
  , datasetDescription :: Maybe Text
  , datasetIssued :: Maybe Day
  , datasetKeywords :: Maybe [Text]
  , datasetLanguage :: Maybe LanguageType
  , datasetPersonalData :: PersonalDataType
  , datasetSensitiveData :: SensitiveDataType
  , datasetReuseDataset :: Maybe Bool
  , datasetTitle :: NonEmptyText
  , datasetType :: Maybe Text
  , datasetDatasetId :: DatasetId
  , datasetDistributions :: [Distribution]
  , datasetMetadata :: [Metadata]
  , datasetRightsRelatedToData :: [RightsRelatedToData]
  , datasetSecurityAndPrivacy :: [SecurityAndPrivacy]
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DatasetId = DatasetId
  { datasetIdIdentifier :: Maybe Text
  , datasetIdType :: DocumentIdType
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Distribution = Distribution
  { distributionAccessUrl :: Maybe Text
  , distributionDataAccess :: Maybe DataAccessType
  , distributionDescription :: Maybe Text
  , distributionDownloadUri :: Maybe Text
  , distributionFormat :: Maybe Text
  , distributionTitle :: NonEmptyText
  , distributionLicenses :: [License]
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Dmp = Dmp
  { dmpId :: Maybe Int
  , dmpCreated :: Maybe UTCTime
  , dmpDescription :: Maybe Text
  , dmpLanguage :: LanguageType
  , dmpModified :: Maybe UTCTime
  , dmpNextReviewDmp :: Maybe Day
  , dmpOrgId :: Text
  , dmpTitle :: NonEmptyText
  , dmpTypeDmp :: DmpType
  , dmpContact :: Contact
  , dmpDmpId :: DmpId
  , dmpContributors :: [Contributor]
  , dmpDataLifeCycle :: Maybe DataLifeCycle
  , dmpDatasets :: [Dataset]
  , dmpEthicalIssues :: [EthicalIssue]
  , dmpProjects :: [Project]
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DmpId = DmpId
  { dmpIdIdentifier :: Maybe Text
  , dmpIdType :: DocumentIdType
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data EthicalIssue = EthicalIssue
  { ethicalIssueDescription :: Maybe Text
  , ethicalIssueExist :: EthicalIssuesType
  , ethicalIssueReport :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data License = License
  { licenseRef :: NonEmptyText
  , licenseStartDate :: Day
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Metadata = Metadata
  { metadataAccessDocumentation :: Maybe Bool
  , metadataDataModel :: Maybe Text
  , metadataDescription :: Maybe Text
  , metadataLanguage :: LanguageType
  , metadataLocationDocumentation :: Maybe Text
  , metadataOpen :: Maybe Bool
  , metadataLocation :: Maybe Text
  , metadataSchema :: Maybe Bool
  , metadataMetadataId :: MetadataId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data MetadataId = MetadataId
  { metadataIdIdentifier :: Maybe Text
  , metadataIdType :: MetadataIdType
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Project = Project
  { projectDescription :: NonEmptyText
  , projectEndDate :: Maybe Day
  , projectStartDate :: Day
  , projectTitle :: NonEmptyText
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data RightsRelatedToData = RightsRelatedToData
  { rightsOwnershipDataRight :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SecurityAndPrivacy = SecurityAndPrivacy
  { securityDescription :: NonEmptyText
  , securityTitle :: NonEmptyText
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

