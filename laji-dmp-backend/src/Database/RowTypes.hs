{-# LANGUAGE ScopedTypeVariables #-}
module Database.RowTypes where

import Database.PostgreSQL.Simple.FromRow
import qualified Database.Models as Models
import Data.Time (Day, UTCTime, parseTimeM, defaultTimeLocale)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Data.Text
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField (ResultError(..))

-- This newtype is a workaround for a psql binary interface timestamp parsing bug
-- In queries we cast the timestamp to text, then use this fromField instance for items
-- and unwrap the newtype later
newtype TextTimestamp = TextTimestamp { unTextTimestamp :: UTCTime } deriving (Show)

instance FromField TextTimestamp where
  fromField f mbs = do
    txt :: Text <- fromField f mbs
    case parseTimeM True defaultTimeLocale "%F %T%Q" (T.unpack txt) of
      Just t  -> pure (TextTimestamp t)
      Nothing -> returnError ConversionFailed f "Could not parse text timestamp"

data DmpJoinRow = DmpJoinRow
  {
    dmpCreated :: TextTimestamp,
    dmpDescription :: Maybe Text,
    dmpId :: Int,
    dmpLanguage :: Models.LanguageType,
    dmpModified :: TextTimestamp,
    dmpNextReviewDmp :: Maybe Day,
    dmpOrgId :: Text,
    dmpTitle :: Text,
    dmpTypeDmp :: Models.DmpType,

    contactId :: Maybe Int,
    contactMbox :: Maybe Text,
    contactName :: Maybe Text,
    contactOrganization :: Maybe Text,

    contactIdsId :: Maybe Int,
    contactIdsIdentifier :: Maybe Text,
    contactIdsType :: Maybe Models.PersonIdType,

    contributorId :: Maybe Int,
    contributorMbox :: Maybe Text,
    contributorName :: Maybe Text,
    contributorOrganization :: Maybe Text,
    contributorRole :: Maybe Models.RoleType,

    contributorIdsId :: Maybe Int,
    contributorIdsIdentifier :: Maybe Text,
    contributorIdsType :: Maybe Models.PersonIdType,

    dataLifeCycleId :: Maybe Int,
    dataLifeCycleArchivingServicesData :: Maybe Bool,
    dataLifeCycleBackupData :: Maybe Text,
    dataLifeCycleDeletionData :: Maybe Models.DeletionDataType,
    dataLifeCycleDeletionWhenData :: Maybe Day,

    datasetId :: Maybe Int,
    datasetDataQualityAssurance :: Maybe Text,
    datasetDataSharingIssues :: Maybe Text,
    datasetDescription :: Maybe Text,
    datasetIssued :: Maybe Day,
    datasetKeywords :: Maybe (PGArray Text),
    datasetLanguage :: Maybe Models.LanguageType,
    datasetPersonalData :: Maybe Models.PersonalDataType,
    datasetSensitiveData :: Maybe Models.SensitiveDataType,
    datasetReuseDataset :: Maybe Bool,
    datasetTitle :: Maybe Text,
    datasetType :: Maybe Text,

    datasetIdsId :: Maybe Int,
    datasetIdsIdentifier :: Maybe Text,
    datasetIdsType :: Maybe Models.DocumentIdType,

    distributionId :: Maybe Int,
    distributionAccessUrl :: Maybe Text,
    distributionDataAccess :: Maybe Models.DataAccessType,
    distributionDescription :: Maybe Text,
    distributionDownloadUri :: Maybe Text,
    distributionFormat :: Maybe Text,
    distributionTitle :: Maybe Text,

    dmpIdsId :: Maybe Int,
    dmpIdsIdentifier :: Maybe Text,
    dmpIdsType :: Maybe Models.DocumentIdType,

    ethicalIssuesId :: Maybe Int,
    ethicalIssuesDescription :: Maybe Text,
    ethicalIssuesExist :: Maybe Models.EthicalIssuesType,
    ethicalIssuesReport :: Maybe Text,

    licenseId :: Maybe Int,
    licensesLicenseRef :: Maybe Text,
    licensesStartDate :: Maybe Day,

    metadataId :: Maybe Int,
    metadataAccessDocumentation :: Maybe Bool,
    metadataDataModel :: Maybe Text,
    metadataDescription :: Maybe Text,
    metadataLanguage :: Maybe Models.LanguageType,
    metadataLocationDocumentation :: Maybe Text,
    metadataMetadataOpen :: Maybe Bool,
    metadataMetadataLocation :: Maybe Text,
    metadataSchema :: Maybe Bool,

    metadataIdsId :: Maybe Int,
    metadataIdsIdentifier :: Maybe Text,
    metadataIdsType :: Maybe Models.MetadataIdType,

    projectId :: Maybe Int,
    projectDescription :: Maybe Text,
    projectEndDate :: Maybe Day,
    projectStartDate :: Maybe Day,
    projectTitle :: Maybe Text,

    rightsRelatedToDataId :: Maybe Int,
    rightsRelatedToDataOwnershipDataRight :: Maybe Text,

    securityAndPrivacyId :: Maybe Int,
    securityAndPrivacyDescription :: Maybe Text,
    securityAndPrivacyTitle :: Maybe Text
  } deriving (Show)

instance FromRow DmpJoinRow where
  fromRow = DmpJoinRow <$>
    field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

