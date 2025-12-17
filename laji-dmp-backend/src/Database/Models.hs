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
  | DataAccessTypeClassified
  | DataAccessTypeEmbargoed
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

data DataType
  = DataTypeCitizenScienceData
  | DataTypeCollection
  | DataTypeFieldObservation
  | DataTypeLaserScanning
  | DataTypeModel
  | DataTypeMolecularBiology
  | DataTypeRemoteSensing
  | DataTypeReport
  | DataTypeSatelliteImagesAndOrtophotos
  | DataTypeSpatialData
  | DataTypeOther
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField DataType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "citizen_science_data" -> return DataTypeCitizenScienceData
    Just "collection" -> return DataTypeCollection
    Just "field_observation" -> return DataTypeFieldObservation
    Just "laser_scanning" -> return DataTypeLaserScanning
    Just "model" -> return DataTypeModel
    Just "molecular_biology" -> return DataTypeMolecularBiology
    Just "remote_sensing" -> return DataTypeRemoteSensing
    Just "report" -> return DataTypeReport
    Just "satellite_images_and_ortophotos" -> return DataTypeSatelliteImagesAndOrtophotos
    Just "spatial_data" -> return DataTypeSpatialData
    Just "other" -> return DataTypeOther
    _ -> returnError ConversionFailed f $ "Invalid value for DataType: " ++ show mbs

instance ToField DataType where
  toField DataTypeCitizenScienceData = toField ("citizen_science_data" :: Text)
  toField DataTypeCollection = toField ("collection" :: Text)
  toField DataTypeFieldObservation = toField ("field_observation" :: Text)
  toField DataTypeLaserScanning = toField ("laser_scanning" :: Text)
  toField DataTypeModel = toField ("model" :: Text)
  toField DataTypeMolecularBiology = toField ("molecular_biology" :: Text)
  toField DataTypeRemoteSensing = toField ("remote_sensing" :: Text)
  toField DataTypeReport = toField ("report" :: Text)
  toField DataTypeSatelliteImagesAndOrtophotos = toField ("satellite_images_and_ortophotos" :: Text)
  toField DataTypeSpatialData = toField ("spatial_data" :: Text)
  toField DataTypeOther = toField ("other" :: Text)

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
  | LanguageTypeOther
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

languageTypeToText :: LanguageType -> Text
languageTypeToText LanguageTypeFi = "fi"
languageTypeToText LanguageTypeSv = "sv"
languageTypeToText LanguageTypeEn = "en"
languageTypeToText LanguageTypeOther = "other"

languageTypeFromText :: Text -> Either String LanguageType
languageTypeFromText "fi" = Right LanguageTypeFi
languageTypeFromText "sv" = Right LanguageTypeSv
languageTypeFromText "en" = Right LanguageTypeEn
languageTypeFromText "other" = Right LanguageTypeOther
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
  = RoleTypeProjectDataController
  | RoleTypeDataOwner
  | RoleTypeOrganizationDataController
  | RoleTypeDatasetAuthor
  | RoleTypeOther
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField RoleType where
  fromField f mbs = case fmap decodeUtf8 mbs of
    Just "project_data_controller" -> return RoleTypeProjectDataController
    Just "data_owner" -> return RoleTypeDataOwner
    Just "organization_data_controller" -> return RoleTypeOrganizationDataController
    Just "dataset_author" -> return RoleTypeDatasetAuthor
    Just "other" -> return RoleTypeOther
    _ -> returnError ConversionFailed f $ "Invalid value for RoleType: " ++ show mbs

instance ToField RoleType where
  toField RoleTypeProjectDataController = toField ("project_data_controller" :: Text)
  toField RoleTypeDataOwner = toField ("data_owner" :: Text)
  toField RoleTypeOrganizationDataController = toField ("organization_data_controller" :: Text)
  toField RoleTypeDatasetAuthor = toField ("dataset_author" :: Text)
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
  , dataLifeCycleDeletionWhenData :: Maybe Day
  , dataLifeCycleUpdateFrequency :: NonEmptyText
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Dataset = Dataset
  { datasetDataQualityAssurance :: Maybe Text
  , datasetDataSharingIssues :: Maybe Text
  , datasetDescription :: Maybe Text
  , datasetIssued :: Maybe Day
  , datasetKeywords :: Maybe [Text]
  , datasetLanguage :: LanguageType
  , datasetPersonalData :: PersonalDataType
  , datasetSensitiveData :: SensitiveDataType
  , datasetReuseDataset :: Maybe Bool
  , datasetTitle :: NonEmptyText
  , datasetVocabulary :: Maybe [Text]
  , datasetResponsiblePartyTitle :: NonEmptyText
  , datasetResponsiblePartyEmail :: NonEmptyText
  , datasetLineage :: Maybe Text
  , datasetShareToSyke :: Bool
  , datasetDataType :: DataType
  , datasetDatasetId :: DatasetId
  , datasetDistributions :: [Distribution]
  , datasetMetadata :: [Metadata]
  , datasetSecurityAndPrivacy :: [SecurityAndPrivacy]
  , datasetDataLifeCycle :: Maybe DataLifeCycle
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
  , dmpModified :: Maybe UTCTime
  , dmpNextReviewDmp :: Maybe Day
  , dmpOrgId :: Text
  , dmpTitle :: NonEmptyText
  , dmpContact :: Contact
  , dmpDmpId :: DmpId
  , dmpContributors :: [Contributor]
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
  { metadataLanguage :: LanguageType
  , metadataOpen :: Maybe Bool
  , metadataLocation :: Maybe Text
  , metadataStandards :: Maybe [Text]
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

data SecurityAndPrivacy = SecurityAndPrivacy
  { securityDescription :: NonEmptyText
  , securityTitle :: NonEmptyText
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
