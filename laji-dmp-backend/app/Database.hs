{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Database
  ( DataManagementPlan
  , createDataManagementPlanTable
  , getDataManagementPlan
  , getDataManagementPlans
  , createDataManagementPlan
  , updateDataManagementPlan
  , deleteDataManagementPlan
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Database.SQLite.Simple
import Data.Swagger

data DataManagementPlan = DataManagementPlan
  { plan_id :: Maybe Int,
    test_field :: String
  } deriving (Generic, Show)

instance ToJSON DataManagementPlan
instance FromJSON DataManagementPlan

instance FromRow DataManagementPlan where
  fromRow = DataManagementPlan <$> field <*> field

instance ToRow DataManagementPlan where
  toRow (DataManagementPlan i t) = toRow (i, t)

instance ToSchema DataManagementPlan where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

createDataManagementPlanTable :: Connection -> IO ()
createDataManagementPlanTable conn = execute_ conn "\
  \CREATE TABLE IF NOT EXISTS data_management_plans (\
  \plan_id INTEGER PRIMARY KEY,\
  \test_field TEXT\
  \);"

getDataManagementPlans :: Connection -> IO [DataManagementPlan]
getDataManagementPlans conn = query_ conn
  "SELECT plan_id, test_field FROM data_management_plans"

createDataManagementPlan :: Connection -> DataManagementPlan -> IO ()
createDataManagementPlan conn plan = execute conn "INSERT INTO data_management_plans (test_field) VALUES (?)" [test_field plan]

getDataManagementPlan :: Connection -> Int -> IO [DataManagementPlan]
getDataManagementPlan conn plan_id = query conn "SELECT * FROM data_management_plans WHERE plan_id = ?" [plan_id]

updateDataManagementPlan :: Connection -> Int -> DataManagementPlan -> IO ()
updateDataManagementPlan conn plan_id (DataManagementPlan _ test_field) =
  execute conn "\
  \UPDATE data_management_plans \
  \SET test_field = ? \
  \WHERE plan_id = ? \
  \" (test_field, plan_id)

deleteDataManagementPlan :: Connection -> Int -> IO [DataManagementPlan]
deleteDataManagementPlan conn plan_id = query conn "DELETE FROM data_management_plans WHERE plan_id = ?" [plan_id]

