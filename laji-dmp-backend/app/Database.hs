{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database
  ( DataManagementPlan
  , createDataManagementPlanTable
  , enableForeignKeys
  , getDataManagementPlan
  , getDataManagementPlans
  , createDataManagementPlan
  , updateDataManagementPlan
  , deleteDataManagementPlan
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Database.SQLite.Simple
import Data.Swagger hiding (title, host, Host)
import Database.SQLite.Simple.FromField
import Prelude
import Data.List (sortOn, groupBy)
import Control.Monad (forM_)
import Database.SQLite.Simple.Ok (Ok)
import Database.SQLite.Simple.ToField
import Data.Maybe (mapMaybe)

data PersonalData = Yes | No | Unknown
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField PersonalData where
  fromField f = do
    value <- fromField f :: Ok String
    case value of
      "No" -> return No
      "Yes" -> return Yes
      _ -> return Unknown

instance ToField PersonalData where
  toField f = toField $ show f

data DataAccess = Open | Shared | Closed
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromField DataAccess where
  fromField f = do
    value <- fromField f :: Ok String
    case value of
      "Open" -> return Open
      "Shared" -> return Shared
      _ -> return Closed

instance ToField DataAccess where
  toField f = toField $ show f

data Host = Host
  { backup_frequency :: Maybe String
  , geo_location :: Maybe String
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Distribution = Distribution
  { data_access :: DataAccess
  , access_url :: Maybe String
  , host :: Maybe Database.Host
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Dataset = Dataset
  { title :: String
  , personal_data :: PersonalData
  , distributions :: [Distribution]
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DataManagementPlan = DataManagementPlan
  { plan_id :: Maybe Int
  , datasets :: [Dataset]
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-------------------
-- CREATE TABLES --
-------------------

createDataManagementPlanTable :: Connection -> IO ()
createDataManagementPlanTable conn =
  let
    createDMPTable = execute_ conn "\
      \CREATE TABLE IF NOT EXISTS data_management_plans (\
      \id INTEGER PRIMARY KEY \
      \);"
    createHostsTable = execute_ conn
      "CREATE TABLE IF NOT EXISTS hosts (\
      \id INTEGER PRIMARY KEY, \
      \distribution_id INTEGER NOT NULL, \
      \backup_frequency TEXT, \
      \geo_location TEXT, \
      \FOREIGN KEY(distribution_id) REFERENCES distributions(id) ON DELETE CASCADE \
      \);"
    createDistributionsTable = execute_ conn
      "CREATE TABLE IF NOT EXISTS distributions (\
      \id INTEGER PRIMARY KEY, \
      \dataset_id INTEGER NOT NULL, \
      \data_access TEXT NOT NULL, \
      \access_url TEXT, \
      \FOREIGN KEY(dataset_id) REFERENCES datasets(id) ON DELETE CASCADE \
      \);"
    createDatasetsTable = execute_ conn "\
      \CREATE TABLE IF NOT EXISTS datasets (\
      \id INTEGER PRIMARY KEY, \
      \dmp_id INTEGER NOT NULL, \
      \title TEXT NOT NULL, \
      \personal_data TEXT NOT NULL, \
      \FOREIGN KEY(dmp_id) REFERENCES data_management_plans(id) ON DELETE CASCADE \
      \);"
  in do
    createDMPTable
    createDatasetsTable
    createDistributionsTable
    createHostsTable

enableForeignKeys :: Connection -> IO ()
enableForeignKeys conn = execute_ conn "PRAGMA foreign_keys = ON;"

-------------------
--- INDEX QUERY ---
-------------------

data DMPQueryResult = DMPQueryResult
  { dmp_plan_id :: Int
  , dataset_id :: Maybe Int
  , datasets_title :: Maybe String
  , datasets_personal_data :: Maybe PersonalData
  , distribution_id :: Maybe Int
  , distributions_data_access :: Maybe DataAccess
  , distributions_access_url :: Maybe String
  , host_id :: Maybe Int
  , hosts_backup_frequency :: Maybe String
  , hosts_geo_location :: Maybe String
  }

instance FromRow DMPQueryResult where
  fromRow = DMPQueryResult <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

groupQueryResultBy :: Ord b => [t] -> (t -> b) -> [[t]]
groupQueryResultBy dmps by = groupBy (\a b -> by a == by b) (sortOn by dmps)

groupByPlanId :: [DMPQueryResult] -> [[DMPQueryResult]]
groupByPlanId dmps = groupQueryResultBy dmps dmp_plan_id

groupByDatasetId :: [DMPQueryResult] -> [[DMPQueryResult]]
groupByDatasetId dmps = groupQueryResultBy dmps dataset_id

groupByDistributionId :: [DMPQueryResult] -> [[DMPQueryResult]]
groupByDistributionId dmps = groupQueryResultBy dmps distribution_id

groupByHostId :: [DMPQueryResult] -> [[DMPQueryResult]]
groupByHostId dmps = groupQueryResultBy dmps host_id

parseHost :: DMPQueryResult -> Maybe Host
parseHost res =
  case host_id res of
  Just _ -> Just $ Host
    { backup_frequency = hosts_backup_frequency res
    , geo_location = hosts_geo_location res
    }
  Nothing -> Nothing

parseDistributions :: [[DMPQueryResult]] -> [Distribution]
parseDistributions dmps =
  let
    f dmp = case distributions_data_access dmp of
      Just da -> Just Distribution
        { data_access = da
        , access_url = distributions_access_url dmp
        , host = parseHost dmp
        }
      _ -> Nothing
  in mapMaybe (f . head) dmps

parseDatasets :: [[DMPQueryResult]] -> [Dataset]
parseDatasets grouped_dmps =
  let
    f dmps =
      let dmp = head dmps
      in case (datasets_title dmp, datasets_personal_data dmp) of
        (Just t, Just p) -> Just Dataset
          { title = t
          , personal_data = p
          , distributions = parseDistributions $ groupByDistributionId dmps
          }
        (_, _) -> Nothing
  in mapMaybe f grouped_dmps

parseDMPs :: [[DMPQueryResult]] -> [DataManagementPlan]
parseDMPs grouped_dmps =
  let
    f dmps =
      let dmp = head dmps
      in DataManagementPlan
        { plan_id = Just $ dmp_plan_id dmp
        , datasets = parseDatasets $ groupByDatasetId dmps
        }
  in fmap f grouped_dmps

dmpQueryResultToNested :: [DMPQueryResult] -> [DataManagementPlan]
dmpQueryResultToNested arr = parseDMPs $ groupByPlanId arr

getDataManagementPlans :: Connection -> IO [DataManagementPlan]
getDataManagementPlans conn = dmpQueryResultToNested <$> query_ conn "\
  \SELECT data_management_plans.id AS plan_id, \
  \       datasets.id AS dataset_id, \
  \       datasets.title AS title, \
  \       datasets.personal_data AS personal_data, \
  \       distributions.id AS distribution_id, \
  \       distributions.data_access AS data_access, \
  \       distributions.access_url AS access_url, \
  \       hosts.id AS host_id, \
  \       hosts.backup_frequency AS backup_frequency, \
  \       hosts.geo_location AS geo_location \
  \FROM data_management_plans \
  \LEFT JOIN datasets ON data_management_plans.id = datasets.dmp_id \
  \LEFT JOIN distributions ON datasets.id = distributions.dataset_id \
  \LEFT JOIN hosts ON distributions.id = hosts.distribution_id;"

--------------------
--- CREATE QUERY ---
--------------------

insertHost :: Connection -> Int -> Host -> IO ()
insertHost conn distributionId (Host backupFrequency geoLocation) =
  execute conn "INSERT INTO hosts (distribution_id, backup_frequency, geo_location) VALUES (?, ?, ?);"
    (distributionId, backupFrequency, geoLocation)

insertDistribution :: Connection -> Int -> Distribution -> IO Int
insertDistribution conn datasetId (Distribution dataAccess accessUrl _) = do
  execute conn "INSERT INTO distributions (dataset_id, data_access, access_url) VALUES (?, ?, ?);"
    (datasetId, dataAccess, accessUrl)
  fromIntegral <$> lastInsertRowId conn

insertDataset :: Connection -> Int -> Dataset -> IO Int
insertDataset conn dmpId (Dataset title personalData _) = do
  execute conn "INSERT INTO datasets (dmp_id, title, personal_data) VALUES (?, ?, ?);"
    (dmpId, title, personalData)
  fromIntegral <$> lastInsertRowId conn

insertDataManagementPlan :: Connection -> DataManagementPlan -> IO Int
insertDataManagementPlan conn (DataManagementPlan _ _) = do
  execute_ conn "INSERT INTO data_management_plans DEFAULT VALUES;"
  fromIntegral <$> lastInsertRowId conn

createDatasets :: Connection -> DataManagementPlan -> Int -> IO ()
createDatasets conn plan dmpId = do
  forM_ (datasets plan) $ \dataset -> do
    datasetId <- insertDataset conn dmpId dataset
    forM_ (distributions dataset) $ \distribution -> do
      distributionId <- insertDistribution conn datasetId distribution
      forM_ (host distribution) (insertHost conn distributionId)

createDataManagementPlan :: Connection -> DataManagementPlan -> IO ()
createDataManagementPlan conn plan = do
  dmpId <- insertDataManagementPlan conn plan
  createDatasets conn plan dmpId

-----------------
--- GET QUERY ---
-----------------

getDataManagementPlan :: Connection -> Int -> IO [DataManagementPlan]
getDataManagementPlan conn planId = dmpQueryResultToNested <$> query conn "\
  \SELECT data_management_plans.id AS plan_id, \
  \       datasets.id AS dataset_id, \
  \       datasets.title AS title, \
  \       datasets.personal_data AS personal_data, \
  \       distributions.id AS distribution_id, \
  \       distributions.data_access AS data_access, \
  \       distributions.access_url AS access_url, \
  \       hosts.id AS host_id, \
  \       hosts.backup_frequency AS backup_frequency, \
  \       hosts.geo_location AS geo_location \
  \FROM data_management_plans \
  \LEFT JOIN datasets ON data_management_plans.id = datasets.dmp_id \
  \LEFT JOIN distributions ON datasets.id = distributions.dataset_id \
  \LEFT JOIN hosts ON distributions.id = hosts.distribution_id \
  \WHERE data_management_plans.id = ?;"
  [planId]

--------------------
--- UPDATE QUERY ---
--------------------

deleteDatasets :: Connection -> Int -> IO ()
deleteDatasets conn planId =
  execute conn "DELETE FROM datasets WHERE dmp_id = ?;" [planId]

updateDataManagementPlan :: Connection -> Int -> DataManagementPlan -> IO ()
updateDataManagementPlan conn planId plan = do
  deleteDatasets conn planId
  createDatasets conn plan planId

--------------------
--- DELETE QUERY ---
--------------------

deleteDataManagementPlan :: Connection -> Int -> IO ()
deleteDataManagementPlan conn planId =
  execute conn "DELETE FROM data_management_plans WHERE id = ?;" [planId]

