{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Queries where

import qualified Database.RowTypes as RowTypes
import qualified Database.Models as Models
import Text.RawString.QQ (r)
import Database.PostgreSQL.Simple (Connection, query_, Only (Only), query, execute, execute_)
import Data.List (groupBy, sortOn)
import Control.Monad (void, forM_)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple.FromField ()
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (FromRow(..), query)
import Database.PostgreSQL.Simple.FromField ()
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Types (PGArray(PGArray, fromPGArray))
import qualified Data.Maybe
import Data.Maybe (listToMaybe)
import Database.Models (NonEmptyText(..))
import qualified Database.Models as Models
import qualified Database.Models as Models

groupQueryResultBy :: Ord b => [t] -> (t -> b) -> [[t]]
groupQueryResultBy dmps by = groupBy (\a b -> by a == by b) (sortOn by dmps)

groupByField
  :: Ord b => (RowTypes.DmpJoinRow -> Maybe b)
  -> [RowTypes.DmpJoinRow]
  -> [[RowTypes.DmpJoinRow]]
groupByField accessor rows =
  let f row = case accessor row of
                Just _  -> True
                Nothing -> False
  in groupQueryResultBy (filter f rows) accessor

groupByDmp :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByDmp = groupByField (Just <$> RowTypes.dmpId)

groupByContributor :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByContributor = groupByField RowTypes.contributorId

groupByContact :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByContact = groupByField RowTypes.contactId

groupByDataLifeCycle :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByDataLifeCycle = groupByField RowTypes.dataLifeCycleId

groupByLicense :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByLicense = groupByField RowTypes.licenseId

groupByDistribution :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByDistribution = groupByField RowTypes.distributionId

groupByMetadata :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByMetadata = groupByField RowTypes.metadataId

groupBySecurityAndPrivacy :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupBySecurityAndPrivacy = groupByField RowTypes.securityAndPrivacyId

groupByEthicalIssue :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByEthicalIssue = groupByField RowTypes.ethicalIssuesId

groupByProject :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByProject = groupByField RowTypes.projectId

groupByDataset :: [RowTypes.DmpJoinRow] -> [[RowTypes.DmpJoinRow]]
groupByDataset = groupByField RowTypes.datasetId

parseContactId :: RowTypes.DmpJoinRow -> Either String Models.ContactId
parseContactId RowTypes.DmpJoinRow {
  RowTypes.contactIdsIdentifier = identifier,
  RowTypes.contactIdsType = Just t
  } = Right $ Models.ContactId identifier t
parseContactId row = Left $ "Could not parse ContactId: " ++ show row

parseContact :: RowTypes.DmpJoinRow -> Either String Models.Contact
parseContact row@RowTypes.DmpJoinRow {
  RowTypes.contactMbox = Just a,
  RowTypes.contactName = Just b,
  RowTypes.contactOrganization = c
  } = Models.Contact (NonEmptyText a) (NonEmptyText b) c <$> parseContactId row
parseContact row = Left $ "Could not parse Contact: " ++ show row

parseContacts :: [RowTypes.DmpJoinRow] -> Either String [Models.Contact]
parseContacts arr = mapM (parseContact . head) (groupByContact arr)

parseDmpId :: RowTypes.DmpJoinRow -> Either String Models.DmpId
parseDmpId RowTypes.DmpJoinRow {
  RowTypes.dmpIdsIdentifier = identifier,
  RowTypes.dmpIdsType = Just t
  } = Right $ Models.DmpId identifier t
parseDmpId row = Left $ "Could not parse DmpId: " ++ show row

parseDmpIds :: [RowTypes.DmpJoinRow] -> Either String [Models.DmpId]
parseDmpIds arr = mapM (parseDmpId . head) (groupByField RowTypes.dmpIdsId arr)

parseContributorId :: RowTypes.DmpJoinRow -> Either String Models.ContributorId
parseContributorId RowTypes.DmpJoinRow {
  RowTypes.contributorIdsIdentifier = identifier,
  RowTypes.contributorIdsType = Just t
  } = Right $ Models.ContributorId identifier t
parseContributorId row = Left $ "Could not parse ContributorId: " ++ show row

parseContributor :: RowTypes.DmpJoinRow -> Either String Models.Contributor
parseContributor row@RowTypes.DmpJoinRow
  { RowTypes.contributorMbox = mbox
  , RowTypes.contributorName = Just name
  , RowTypes.contributorOrganization = org
  , RowTypes.contributorRole = Just role
  } = Models.Contributor mbox (NonEmptyText name) org role <$> parseContributorId row
parseContributor row =
  Left $ "Could not parse Contributor: " ++ show row

parseContributors :: [RowTypes.DmpJoinRow] -> Either String [Models.Contributor]
parseContributors arr = mapM (parseContributor . head) (groupByContributor arr)

parseDataLifeCycle :: RowTypes.DmpJoinRow -> Either String Models.DataLifeCycle
parseDataLifeCycle RowTypes.DmpJoinRow {
  RowTypes.dataLifeCycleArchivingServicesData = Just a,
  RowTypes.dataLifeCycleBackupData = Just b,
  RowTypes.dataLifeCycleDeletionWhenData = c,
  RowTypes.dataLifeCycleUpdateFrequency = Just d
  } = Right $ Models.DataLifeCycle a (NonEmptyText b) c (NonEmptyText d)
parseDataLifeCycle row = Left $ "Could not parse DateLifeCycle: " ++ show row

parseDataLifeCycles :: [RowTypes.DmpJoinRow] -> Either String [Models.DataLifeCycle]
parseDataLifeCycles arr = mapM (parseDataLifeCycle . head) (groupByDataLifeCycle arr)

parseLicense :: RowTypes.DmpJoinRow -> Either String Models.License
parseLicense RowTypes.DmpJoinRow
  { RowTypes.licensesLicenseRef = Just a
  , RowTypes.licensesStartDate = Just b
  } = Right $ Models.License (NonEmptyText a) b
parseLicense row =
  Left $ "Could not parse License: " ++ show row

parseLicenses :: [RowTypes.DmpJoinRow] -> Either String [Models.License]
parseLicenses arr = mapM (parseLicense . head) (groupByLicense arr)

parseDistribution :: [RowTypes.DmpJoinRow] -> Either String Models.Distribution
parseDistribution rows = case head rows of
  RowTypes.DmpJoinRow
    { RowTypes.distributionAccessUrl = a
    , RowTypes.distributionDataAccess = b
    , RowTypes.distributionDescription = c
    , RowTypes.distributionDownloadUri = d
    , RowTypes.distributionFormat = e
    , RowTypes.distributionTitle = Just f
    } -> Models.Distribution a b c d e (NonEmptyText f) <$> parseLicenses rows
  row -> Left $ "Could not parse Distribution: " ++ show row

parseDistributions :: [RowTypes.DmpJoinRow] -> Either String [Models.Distribution]
parseDistributions arr = mapM parseDistribution (groupByDistribution arr)

parseMetadataId :: RowTypes.DmpJoinRow -> Either String Models.MetadataId
parseMetadataId RowTypes.DmpJoinRow {
  RowTypes.metadataIdsIdentifier = identifier,
  RowTypes.metadataIdsType = Just t
  } = Right $ Models.MetadataId identifier t
parseMetadataId row = Left $ "Could not parse MetadataId: " ++ show row

parseMetadata :: RowTypes.DmpJoinRow -> Either String Models.Metadata
parseMetadata row@RowTypes.DmpJoinRow
  { RowTypes.metadataLanguage = Just b
  , RowTypes.metadataMetadataOpen = c
  , RowTypes.metadataMetadataLocation = d
  , RowTypes.metadataStandards = e
  } = Models.Metadata b c d (fmap fromPGArray e) <$> parseMetadataId row
parseMetadata row =
  Left $ "Could not parse Metadata: " ++ show row

parseMetadatas :: [RowTypes.DmpJoinRow] -> Either String [Models.Metadata]
parseMetadatas arr = mapM (parseMetadata . head) (groupByMetadata arr)

parseSecurityAndPrivacy :: RowTypes.DmpJoinRow -> Either String Models.SecurityAndPrivacy
parseSecurityAndPrivacy RowTypes.DmpJoinRow
  { RowTypes.securityAndPrivacyDescription = Just a
  , RowTypes.securityAndPrivacyTitle = Just b
  } = Right $ Models.SecurityAndPrivacy (NonEmptyText a) (NonEmptyText b)
parseSecurityAndPrivacy row =
  Left $ "Could not parse SecurityAndPrivacy: " ++ show row

parseSecurityAndPrivacyArr :: [RowTypes.DmpJoinRow] -> Either String [Models.SecurityAndPrivacy]
parseSecurityAndPrivacyArr arr = mapM (parseSecurityAndPrivacy . head) (groupBySecurityAndPrivacy arr)

parseDatasetId :: RowTypes.DmpJoinRow -> Either String Models.DatasetId
parseDatasetId RowTypes.DmpJoinRow {
  RowTypes.datasetIdsIdentifier = identifier,
  RowTypes.datasetIdsType = Just t
  } = Right $ Models.DatasetId identifier t
parseDatasetId row = Left $ "Could not parse DatasetId: " ++ show row

parseDataset :: [RowTypes.DmpJoinRow] -> Either String Models.Dataset
parseDataset rows =
  let
    parseRow row@RowTypes.DmpJoinRow
      { RowTypes.datasetDataQualityAssurance = a
      , RowTypes.datasetDataSharingIssues = b
      , RowTypes.datasetDescription = c
      , RowTypes.datasetIssued = d
      , RowTypes.datasetKeywords = e
      , RowTypes.datasetLanguage = Just f
      , RowTypes.datasetPersonalData = Just g
      , RowTypes.datasetSensitiveData = Just h
      , RowTypes.datasetReuseDataset = i
      , RowTypes.datasetTitle = Just j
      , RowTypes.datasetType = k
      , RowTypes.datasetVocabulary = l
      } = do
        datasetId <- parseDatasetId row
        distributions <- parseDistributions rows
        metadatas <- parseMetadatas rows
        security <- parseSecurityAndPrivacyArr rows
        dataLifeCycles <- parseDataLifeCycles rows
        return $ Models.Dataset a b c d (fmap fromPGArray e) f g h i (NonEmptyText j) k (fmap fromPGArray l) datasetId distributions metadatas security (listToMaybe dataLifeCycles)

    parseRow row =
      Left $ "Could not parse Dataset: " ++ show row
  in parseRow $ head rows

parseDatasets :: [RowTypes.DmpJoinRow] -> Either String [Models.Dataset]
parseDatasets arr = mapM parseDataset (groupByDataset arr)

parseEthicalIssue :: [RowTypes.DmpJoinRow] -> Either String Models.EthicalIssue
parseEthicalIssue rows = case head rows of
  RowTypes.DmpJoinRow
    { RowTypes.ethicalIssuesDescription = a
    , RowTypes.ethicalIssuesExist = Just b
    , RowTypes.ethicalIssuesReport = c
    } -> Right $ Models.EthicalIssue a b c
  row -> Left $ "Could not parse EthicalIssue: " ++ show row

parseEthicalIssues :: [RowTypes.DmpJoinRow] -> Either String [Models.EthicalIssue]
parseEthicalIssues arr = mapM parseEthicalIssue (groupByEthicalIssue arr)

parseProject :: [RowTypes.DmpJoinRow] -> Either String Models.Project
parseProject rows = case head rows of
  RowTypes.DmpJoinRow
    { RowTypes.projectDescription = Just a
    , RowTypes.projectEndDate = b
    , RowTypes.projectStartDate = Just c
    , RowTypes.projectTitle = Just d
    } -> Right $ Models.Project (NonEmptyText a) b c (NonEmptyText d)
  row -> Left $ "Could not parse Project: " ++ show row

parseProjects :: [RowTypes.DmpJoinRow] -> Either String [Models.Project]
parseProjects arr = mapM parseProject (groupByProject arr)

parseDmp :: [RowTypes.DmpJoinRow] -> Either String Models.Dmp
parseDmp rows =
  let
    parseRow :: RowTypes.DmpJoinRow -> Either String Models.Dmp
    parseRow row@RowTypes.DmpJoinRow
      { RowTypes.dmpId = a
      , RowTypes.dmpOrgId = orgId
      , RowTypes.dmpCreated = created
      , RowTypes.dmpDescription = desc
      , RowTypes.dmpLanguage = lang
      , RowTypes.dmpModified = modified
      , RowTypes.dmpNextReviewDmp = nextReview
      , RowTypes.dmpTitle = title
      , RowTypes.dmpTypeDmp = typ
      } = do
        contacts <- parseContacts rows
        dmpIds <- parseDmpIds rows
        contributors <- parseContributors rows
        datasets <- parseDatasets rows
        ethicalIssues <- parseEthicalIssues rows
        projects <- parseProjects rows
        return $ Models.Dmp
          (Just a)
          (Just $ RowTypes.unTextTimestamp created)
          desc
          lang
          (Just $ RowTypes.unTextTimestamp modified)
          nextReview
          orgId
          (NonEmptyText title)
          typ
          (head contacts)
          (head dmpIds)
          contributors
          datasets
          ethicalIssues
          projects
  in parseRow $ head rows

parseDmps :: [RowTypes.DmpJoinRow] -> Either String [Models.Dmp]
parseDmps arr = mapM parseDmp (groupByDmp arr)

getDataManagementPlans :: Connection -> IO (Either String [Models.Dmp])
getDataManagementPlans conn = parseDmps <$> query_ conn [r|
SELECT
  dmps.created::text AS created,
  dmps.description AS description,
  dmps.id AS id,
  dmps.language AS language,
  dmps.modified::text AS modified,
  dmps.nextreview_dmp AS nextreview_dmp,
  dmps.org_id AS org_id,
  dmps.title AS title,
  dmps.type_dmp AS type_dmp,

  contacts.id AS contact_id,
  contacts.mbox AS contact_mbox,
  contacts.name AS contact_name,
  contacts.organization AS contact_organization,

  contact_ids.id AS contact_ids_id,
  contact_ids.identifier AS contact_ids_identifier,
  contact_ids.type AS contact_ids_type,

  contributors.id AS contributor_id,
  contributors.mbox AS contributor_mbox,
  contributors.name AS contributor_name,
  contributors.organization AS contributor_organization,
  contributors.role AS contributor_role,

  contributor_ids.id AS contributor_ids_id,
  contributor_ids.identifier AS contributor_ids_identifier,
  contributor_ids.type AS contributor_ids_type,

  data_life_cycles.id AS data_life_cycle_id,
  data_life_cycles.archiving_services_data AS data_life_cycle_archiving_services_data,
  data_life_cycles.backup_data AS data_life_cycle_backup_data,
  data_life_cycles.deletion_when_data AS data_life_cycle_deletion_when_data,
  data_life_cycles.update_frequency AS data_life_cycle_update_frequency,

  datasets.id AS dataset_id,
  datasets.data_quality_assurance AS datasets_data_quality_assurance,
  datasets.data_sharing_issues AS datasets_data_sharing_issues,
  datasets.description AS datasets_description,
  datasets.issued AS datasets_issued,
  datasets.keywords AS datasets_keywords,
  datasets.language AS datasets_language,
  datasets.personal_data AS datasets_personal_data,
  datasets.sensitive_data AS datasets_sensitive_data,
  datasets.reuse_dataset AS datasets_reuse_dataset,
  datasets.title AS datasets_title,
  datasets.type AS datasets_type,
  datasets.vocabulary AS datasets_vocabulary,

  dataset_ids.id AS dataset_ids_id,
  dataset_ids.identifier AS dataset_ids_identifier,
  dataset_ids.type AS dataset_ids_type,

  distributions.id AS distribution_id,
  distributions.access_url AS distributions_access_url,
  distributions.data_access AS distributions_data_access,
  distributions.description AS distributions_description,
  distributions.download_uri AS distributions_download_uri,
  distributions.format AS distributions_format,
  distributions.title AS distributions_title,

  dmp_ids.id AS dmp_ids_id,
  dmp_ids.identifier AS dmp_ids_identifier,
  dmp_ids.type AS dmp_ids_type,

  ethical_issues.id AS ethical_issues_id,
  ethical_issues.ethical_issues_description AS ethical_issues_description,
  ethical_issues.ethical_issues_exist AS ethical_issues_exist,
  ethical_issues.ethical_issues_report AS ethical_issues_report,

  licenses.id AS license_id,
  licenses.license_ref AS licenses_license_ref,
  licenses.start_date AS licenses_start_date,

  metadata.id AS metadata_id,
  metadata.language AS metadata_language,
  metadata.metadata_open AS metadata_metadata_open,
  metadata.metadata_location AS metadata_metadata_location,
  metadata.standards AS metadata_standards,

  metadata_ids.id AS metadata_ids_id,
  metadata_ids.identifier AS metadata_ids_identifier,
  metadata_ids.type AS metadata_ids_type,

  projects.id AS project_id,
  projects.description AS project_description,
  projects.end_date AS project_end_date,
  projects.start_date AS project_start_date,
  projects.title AS project_title,

  security_and_privacy.id AS security_and_privacy_id,
  security_and_privacy.description AS security_and_privacy_description,
  security_and_privacy.title AS security_and_privacy_title

FROM dmps
LEFT JOIN contacts ON dmps.id = contacts.dmp_id
LEFT JOIN contact_ids ON contacts.id = contact_ids.contact_id
LEFT JOIN contributors ON dmps.id = contributors.dmp_id
LEFT JOIN contributor_ids ON contributors.id = contributor_ids.contributor_id
LEFT JOIN datasets ON dmps.id = datasets.dmp_id
LEFT JOIN data_life_cycles ON datasets.id = data_life_cycles.dataset_id
LEFT JOIN dataset_ids ON datasets.id = dataset_ids.dataset_id
LEFT JOIN distributions ON datasets.id = distributions.dataset_id
LEFT JOIN dmp_ids ON dmps.id = dmp_ids.dmp_id
LEFT JOIN ethical_issues ON dmps.id = ethical_issues.dmp_id
LEFT JOIN licenses ON distributions.id = licenses.distribution_id
LEFT JOIN metadata ON datasets.id = metadata.dataset_id
LEFT JOIN metadata_ids ON metadata.id = metadata_ids.metadata_id
LEFT JOIN projects ON dmps.id = projects.dmp_id
LEFT JOIN security_and_privacy ON datasets.id = security_and_privacy.dataset_id;
|]

getDataManagementPlan :: Connection -> Int -> IO (Either String [Models.Dmp])
getDataManagementPlan conn dmpId = parseDmps <$> query conn [r|
SELECT
  dmps.created::text AS created,
  dmps.description AS description,
  dmps.id AS id,
  dmps.language AS language,
  dmps.modified::text AS modified,
  dmps.nextreview_dmp AS nextreview_dmp,
  dmps.org_id AS org_id,
  dmps.title AS title,
  dmps.type_dmp AS type_dmp,

  contacts.id AS contact_id,
  contacts.mbox AS contact_mbox,
  contacts.name AS contact_name,
  contacts.organization AS contact_organization,

  contact_ids.id AS contact_ids_id,
  contact_ids.identifier AS contact_ids_identifier,
  contact_ids.type AS contact_ids_type,

  contributors.id AS contributor_id,
  contributors.mbox AS contributor_mbox,
  contributors.name AS contributor_name,
  contributors.organization AS contributor_organization,
  contributors.role AS contributor_role,

  contributor_ids.id AS contributor_ids_id,
  contributor_ids.identifier AS contributor_ids_identifier,
  contributor_ids.type AS contributor_ids_type,

  data_life_cycles.id AS data_life_cycle_id,
  data_life_cycles.archiving_services_data AS data_life_cycle_archiving_services_data,
  data_life_cycles.backup_data AS data_life_cycle_backup_data,
  data_life_cycles.deletion_when_data AS data_life_cycle_deletion_when_data,
  data_life_cycles.update_frequency AS data_life_cycle_update_frequency,

  datasets.id AS dataset_id,
  datasets.data_quality_assurance AS datasets_data_quality_assurance,
  datasets.data_sharing_issues AS datasets_data_sharing_issues,
  datasets.description AS datasets_description,
  datasets.issued AS datasets_issued,
  datasets.keywords AS datasets_keywords,
  datasets.language AS datasets_language,
  datasets.personal_data AS datasets_personal_data,
  datasets.sensitive_data AS datasets_sensitive_data,
  datasets.reuse_dataset AS datasets_reuse_dataset,
  datasets.title AS datasets_title,
  datasets.type AS datasets_type,
  datasets.vocabulary AS datasets_vocabulary,

  dataset_ids.id AS dataset_ids_id,
  dataset_ids.identifier AS dataset_ids_identifier,
  dataset_ids.type AS dataset_ids_type,

  distributions.id AS distribution_id,
  distributions.access_url AS distributions_access_url,
  distributions.data_access AS distributions_data_access,
  distributions.description AS distributions_description,
  distributions.download_uri AS distributions_download_uri,
  distributions.format AS distributions_format,
  distributions.title AS distributions_title,

  dmp_ids.id AS dmp_ids_id,
  dmp_ids.identifier AS dmp_ids_identifier,
  dmp_ids.type AS dmp_ids_type,

  ethical_issues.id AS ethical_issues_id,
  ethical_issues.ethical_issues_description AS ethical_issues_description,
  ethical_issues.ethical_issues_exist AS ethical_issues_exist,
  ethical_issues.ethical_issues_report AS ethical_issues_report,

  licenses.id AS license_id,
  licenses.license_ref AS licenses_license_ref,
  licenses.start_date AS licenses_start_date,

  metadata.id AS metadata_id,
  metadata.language AS metadata_language,
  metadata.metadata_open AS metadata_metadata_open,
  metadata.metadata_location AS metadata_metadata_location,
  metadata.standards AS metadata_standards,

  metadata_ids.id AS metadata_ids_id,
  metadata_ids.identifier AS metadata_ids_identifier,
  metadata_ids.type AS metadata_ids_type,

  projects.id AS project_id,
  projects.description AS project_description,
  projects.end_date AS project_end_date,
  projects.start_date AS project_start_date,
  projects.title AS project_title,

  security_and_privacy.id AS security_and_privacy_id,
  security_and_privacy.description AS security_and_privacy_description,
  security_and_privacy.title AS security_and_privacy_title

FROM dmps
LEFT JOIN contacts ON dmps.id = contacts.dmp_id
LEFT JOIN contact_ids ON contacts.id = contact_ids.contact_id
LEFT JOIN contributors ON dmps.id = contributors.dmp_id
LEFT JOIN contributor_ids ON contributors.id = contributor_ids.contributor_id
LEFT JOIN datasets ON dmps.id = datasets.dmp_id
LEFT JOIN data_life_cycles ON datasets.id = data_life_cycles.dataset_id
LEFT JOIN dataset_ids ON datasets.id = dataset_ids.dataset_id
LEFT JOIN distributions ON datasets.id = distributions.dataset_id
LEFT JOIN dmp_ids ON dmps.id = dmp_ids.dmp_id
LEFT JOIN ethical_issues ON dmps.id = ethical_issues.dmp_id
LEFT JOIN licenses ON distributions.id = licenses.distribution_id
LEFT JOIN metadata ON datasets.id = metadata.dataset_id
LEFT JOIN metadata_ids ON metadata.id = metadata_ids.metadata_id
LEFT JOIN projects ON dmps.id = projects.dmp_id
LEFT JOIN security_and_privacy ON datasets.id = security_and_privacy.dataset_id
WHERE dmps.id = ?;
|]
  [dmpId]

insertContactId :: Connection -> Models.ContactId -> Int -> IO Int
insertContactId conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO contact_ids (contact_id, identifier, type) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.contactIdIdentifier self
    , Models.contactIdType self
    )
  return i

insertContact :: Connection -> Models.Contact -> Int -> IO Int
insertContact conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO contacts (dmp_id, mbox, name, organization) VALUES (?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.contactMbox self
    , Models.contactName self
    , Models.contactOrganization self
    )
  _ <- insertContactId conn (Models.contactContactId self) i
  return i

insertContributorId :: Connection -> Models.ContributorId -> Int -> IO Int
insertContributorId conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO contributor_ids (contributor_id, identifier, type) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.contributorIdIdentifier self
    , Models.contributorIdType self
    )
  return i

insertContributor :: Connection -> Models.Contributor -> Int -> IO Int
insertContributor conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO contributors (dmp_id, mbox, name, organization, role) VALUES (?, ?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.contributorMbox self
    , Models.contributorName self
    , Models.contributorOrganization self
    , Models.contributorRole self
    )
  _ <- insertContributorId conn (Models.contributorContributorId self) i
  return i

insertDataLifeCycle :: Connection -> Models.DataLifeCycle -> Int -> IO Int
insertDataLifeCycle conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO data_life_cycles (dataset_id, archiving_services_data, backup_data, deletion_when_data, update_frequency) VALUES (?, ?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.dataLifeCycleArchivingServicesData self
    , Models.dataLifeCycleBackupData self
    , Models.dataLifeCycleDeletionWhenData self
    , Models.dataLifeCycleUpdateFrequency self
    )
  return i

insertDatasetId :: Connection -> Models.DatasetId -> Int -> IO Int
insertDatasetId conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO dataset_ids (dataset_id, identifier, type) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.datasetIdIdentifier self
    , Models.datasetIdType self
    )
  return i

insertDataset :: Connection -> Models.Dataset -> Int -> IO Int
insertDataset conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO datasets (
  dmp_id, data_quality_assurance, data_sharing_issues, description, issued, keywords,
  language, personal_data, sensitive_data, reuse_dataset, title, type, vocabulary
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.datasetDataQualityAssurance self
    , Models.datasetDataSharingIssues self
    , Models.datasetDescription self
    , Models.datasetIssued self
    , PGArray <$> Models.datasetKeywords self
    , Models.datasetLanguage self
    , Models.datasetPersonalData self
    , Models.datasetSensitiveData self
    , Models.datasetReuseDataset self
    , Models.datasetTitle self
    , Models.datasetType self
    , PGArray <$> Models.datasetVocabulary self
    )
  forM_ (Models.datasetDistributions self) (\a -> insertDistribution conn a i)
  forM_ (Models.datasetMetadata self) (\a -> insertMetadata conn a i)
  forM_ (Models.datasetSecurityAndPrivacy self) (\a -> insertSecurityAndPrivacy conn a i)
  forM_ (Models.datasetDataLifeCycle self) (\a -> insertDataLifeCycle conn a i)
  _ <- insertDatasetId conn (Models.datasetDatasetId self) i
  return i

insertDistribution :: Connection -> Models.Distribution -> Int -> IO Int
insertDistribution conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO distributions (dataset_id, access_url, data_access, description, download_uri, format, title) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.distributionAccessUrl self
    , Models.distributionDataAccess self
    , Models.distributionDescription self
    , Models.distributionDownloadUri self
    , Models.distributionFormat self
    , Models.distributionTitle self
    )
  forM_ (Models.distributionLicenses self) (\a -> insertLicense conn a i)
  return i

insertDmpId :: Connection -> Models.DmpId -> Int -> IO Int
insertDmpId conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO dmp_ids (dmp_id, identifier, type) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.dmpIdIdentifier self
    , Models.dmpIdType self
    )
  return i

insertEthicalIssue :: Connection -> Models.EthicalIssue -> Int -> IO Int
insertEthicalIssue conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO ethical_issues (dmp_id, ethical_issues_description, ethical_issues_exist, ethical_issues_report) VALUES (?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.ethicalIssueDescription self
    , Models.ethicalIssueExist self
    , Models.ethicalIssueReport self
    )
  return i

insertLicense :: Connection -> Models.License -> Int -> IO Int
insertLicense conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO licenses (distribution_id, license_ref, start_date) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.licenseRef self
    , Models.licenseStartDate self
    )
  return i

insertMetadata :: Connection -> Models.Metadata -> Int -> IO Int
insertMetadata conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO metadata (
  dataset_id, language,
  metadata_open, metadata_location, standards
) VALUES (?, ?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.metadataLanguage self
    , Models.metadataOpen self
    , Models.metadataLocation self
    , PGArray <$> Models.metadataStandards self
    )
  _ <- insertMetadataId conn (Models.metadataMetadataId self) i
  return i

insertMetadataId :: Connection -> Models.MetadataId -> Int -> IO Int
insertMetadataId conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO metadata_ids (metadata_id, identifier, type) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.metadataIdIdentifier self
    , Models.metadataIdType self
    )
  return i

insertProject :: Connection -> Models.Project -> Int -> IO Int
insertProject conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO projects (dmp_id, description, end_date, start_date, title) VALUES (?, ?, ?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.projectDescription self
    , Models.projectEndDate self
    , Models.projectStartDate self
    , Models.projectTitle self
    )
  return i

insertSecurityAndPrivacy :: Connection -> Models.SecurityAndPrivacy -> Int -> IO Int
insertSecurityAndPrivacy conn self parentId = do
  [Only i] <- query conn [r|
INSERT INTO security_and_privacy (dataset_id, description, title) VALUES (?, ?, ?) RETURNING id;
  |]
    ( parentId
    , Models.securityDescription self
    , Models.securityTitle self
    )
  return i

insertDataManagementPlan :: Connection -> Models.Dmp -> IO Int
insertDataManagementPlan conn self = do
  now <- getCurrentTime
  [Only i] <- query conn [r|
INSERT INTO dmps
(created, description, language, modified, nextreview_dmp, org_id, title, type_dmp)
VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id;
  |]
    ( Data.Maybe.fromMaybe now (Models.dmpCreated self)
    , Models.dmpDescription self
    , Models.dmpLanguage self
    , now
    , Models.dmpNextReviewDmp self
    , Models.dmpOrgId self
    , Models.dmpTitle self
    , Models.dmpTypeDmp self
    )
  forM_ (Models.dmpContributors self) (\a -> insertContributor conn a i)
  forM_ (Models.dmpDatasets self) (\a -> insertDataset conn a i)
  forM_ (Models.dmpEthicalIssues self) (\a -> insertEthicalIssue conn a i)
  forM_ (Models.dmpProjects self) (\a -> insertProject conn a i)
  _ <- insertDmpId conn (Models.dmpDmpId self) i
  _ <- insertContact conn (Models.dmpContact self) i
  return i

createDmp :: Connection -> Models.Dmp -> IO ()
createDmp conn dmp = do
  _ <- insertDataManagementPlan conn dmp
  return ()

deleteContacts :: Connection -> Int -> IO ()
deleteContacts conn parentId =
  void $ execute conn [r|
DELETE FROM contacts WHERE dmp_id = ?;
  |] [parentId]

deleteContributors :: Connection -> Int -> IO ()
deleteContributors conn parentId =
  void $ execute conn [r|
DELETE FROM contributors WHERE dmp_id = ?;
  |] [parentId]

deleteDataLifeCycles :: Connection -> Int -> IO ()
deleteDataLifeCycles conn parentId =
  void $ execute conn [r|
DELETE FROM data_life_cycles WHERE dataset_id = ?;
  |] [parentId]

deleteDatasets :: Connection -> Int -> IO ()
deleteDatasets conn parentId =
  void $ execute conn [r|
DELETE FROM datasets WHERE dmp_id = ?;
  |] [parentId]

deleteDmpIds :: Connection -> Int -> IO ()
deleteDmpIds conn parentId =
  void $ execute conn [r|
DELETE FROM dmp_ids WHERE dmp_id = ?;
  |] [parentId]

deleteEthicalIssues :: Connection -> Int -> IO ()
deleteEthicalIssues conn parentId =
  void $ execute conn [r|
DELETE FROM ethical_issues WHERE dmp_id = ?;
  |] [parentId]

deleteProjects :: Connection -> Int -> IO ()
deleteProjects conn parentId =
  void $ execute conn [r|
DELETE FROM projects WHERE dmp_id = ?;
  |] [parentId]

updateDataManagementPlan :: Connection -> Int -> Models.Dmp -> IO ()
updateDataManagementPlan conn i self = do
  void $ execute conn [r|
UPDATE dmps SET
  created = ?,
  description = ?,
  language = ?,
  modified = ?,
  nextreview_dmp = ?,
  org_id = ?,
  title = ?,
  type_dmp = ?
WHERE id = ?;
  |]
    ( Models.dmpCreated self
    , Models.dmpDescription self
    , Models.dmpLanguage self
    , Models.dmpModified self
    , Models.dmpNextReviewDmp self
    , Models.dmpOrgId self
    , Models.dmpTitle self
    , Models.dmpTypeDmp self
    , i
    )
  _ <- deleteContacts conn i
  _ <- insertContact conn (Models.dmpContact self) i
  _ <- deleteDmpIds conn i
  _ <- insertDmpId conn (Models.dmpDmpId self) i
  _ <- deleteContributors conn i
  forM_ (Models.dmpContributors self) (\a -> insertContributor conn a i)
  _ <- deleteDatasets conn i
  forM_ (Models.dmpDatasets self) (\a -> insertDataset conn a i)
  _ <- deleteEthicalIssues conn i
  forM_ (Models.dmpEthicalIssues self) (\a -> insertEthicalIssue conn a i)
  _ <- deleteProjects conn i
  forM_ (Models.dmpProjects self) (\a -> insertProject conn a i)

deleteDataManagementPlan :: Connection -> Int -> IO ()
deleteDataManagementPlan conn i =
  void $ execute conn [r|
DELETE FROM dmps WHERE id = ?;
  |] [i]

