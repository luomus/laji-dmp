{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (status404)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Servant
import qualified Data.HashMap.Strict as HM

import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

import qualified Database.Models as Models
import qualified Database.Queries as Queries
import LajiApi
import System.Environment (lookupEnv)
import qualified Data.Maybe
import Data.Functor ((<&>))
import Data.Text (Text, unpack, pack)
import Control.Concurrent.STM
import Network.HTTP.Req (HttpConfig)
import Data.Time (UTCTime(UTCTime))
import Database.PostgreSQL.Simple.FromField (Oid (Oid))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.List.Split (splitOn)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Database.Models
import qualified Data.ByteString.Lazy.Char8
import Text.Read (readMaybe)
import Database.PostgreSQL.Simple.Migration (runMigration, defaultOptions, MigrationCommand(MigrationDirectory, MigrationInitialization))
import qualified Data.String as DS
import Data.Coerce (coerce)

data AppState = AppState
  { appDbConnection :: Connection
  , appPersonCache :: TVar (HM.HashMap Text (CacheElement Person))
  , appHttpConfig :: HttpConfig
  , appLajiApiConfig :: LajiApiConfig
  }

checkUserRights :: Text -> Text -> AppState -> IO Bool
checkUserRights personToken orgId state =
  let
    checkOrg :: Person -> Bool
    checkOrg person =
      (case organisation person of
        Just orgs -> unpack orgId `elem` orgs
        Nothing -> False
      )
      ||
      (case organisationAdmin person of
        Just orgs -> unpack orgId `elem` orgs
        Nothing -> False
      )
  in
    do
      maybePerson <- getPerson personToken (appPersonCache state) (appLajiApiConfig state) (appHttpConfig state)
      return $ case maybePerson of
        Just person -> checkOrg person || "MA.admin" `elem` role person
        Nothing -> False

handlerDmpIndex :: AppState -> Handler [Models.Dmp]
handlerDmpIndex (AppState { appDbConnection = conn }) = do
  res <- liftIO $ Queries.getDataManagementPlans conn
  case res of
    Right plans -> return plans
    Left err -> throwError err500 { errBody = encodeUtf8 $ Text.Lazy.pack err }

handlerDmpPost :: AppState -> Text -> Models.Dmp -> Handler NoContent
handlerDmpPost state personToken dmp = do
  userHasRights <- liftIO $ checkUserRights personToken (coerce $ Models.dmpOrgId dmp) state
  if userHasRights
    then do
      liftIO $ Queries.insertDataManagementPlan (appDbConnection state) dmp
      return NoContent
    else do
      throwError err403 { errBody = "User doesn't have rights for this organization." }

handlerDmpGet :: AppState -> Int -> Handler Models.Dmp
handlerDmpGet (AppState { appDbConnection = conn }) i = do
  res <- liftIO $ Queries.getDataManagementPlan conn i
  case res of
    Right plans ->
      case Data.Maybe.listToMaybe plans of
        Just dmp -> return dmp
        Nothing -> throwError err404 { errBody = "No such DMP was found." }
    Left err -> throwError err500 { errBody = encodeUtf8 $ Text.Lazy.pack err }

handlerDmpPut :: AppState -> Text -> Int -> Models.Dmp -> Handler NoContent
handlerDmpPut state personToken dmpId dmp =
  let
    idMatch = case Models.dmpId dmp of
      Just i -> i == dmpId
      Nothing -> True
  in do
    orgMatch <- do
      oldPlanRes <- liftIO $ Queries.getDataManagementPlan (appDbConnection state) dmpId
      case oldPlanRes of
        Right oldPlan ->
          return $ Models.dmpOrgId (head oldPlan) == Models.dmpOrgId dmp
        Left err -> throwError err500 { errBody = encodeUtf8 $ Text.Lazy.pack err }
    userHasRights <- liftIO $ checkUserRights personToken (coerce $ Models.dmpOrgId dmp) state
    if idMatch
      then if orgMatch
        then if userHasRights
          then do
            liftIO $ Queries.updateDataManagementPlan (appDbConnection state) dmpId dmp
            return NoContent
          else do
            throwError err401 { errBody = "User doesn't have rights for this organization." }
        else do
          throwError err403 { errBody = "DMPs can't be transferred between organizations." }
      else do
        throwError err403 { errBody = "Id mismatch." }

handlerDmpDelete :: AppState -> Text -> Int -> Handler NoContent
handlerDmpDelete state personToken dmpId = do
  dmpsResult <- liftIO $ Queries.getDataManagementPlan (appDbConnection state) dmpId
  case dmpsResult of
    Left err -> throwError err404 { errBody = Data.ByteString.Lazy.Char8.pack $ "Could not find DMP: " ++ err }
    Right dmps -> do
      userHasRights <- liftIO $ checkUserRights personToken (coerce $ Database.Models.dmpOrgId $ head dmps) state
      if userHasRights
        then do
          liftIO $ Queries.deleteDataManagementPlan (appDbConnection state) dmpId
          return NoContent
        else do
          throwError err403 { errBody = "User doesn't have rights for this organization." }

handlerNotFound :: Application
handlerNotFound _ respond = respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Route Not Found"

type API =
  "dmp" :>
    (     Get '[JSON] [Models.Dmp]
    :<|>  QueryParam' '[Required] "personToken" Text :> ReqBody '[JSON] Models.Dmp :> Post '[JSON] NoContent
    :<|>  Capture "id" Int :> Get '[JSON] Models.Dmp
    :<|>  QueryParam' '[Required] "personToken" Text :> Capture "id" Int :> ReqBody '[JSON] Models.Dmp :> Put '[JSON] NoContent
    :<|>  QueryParam' '[Required] "personToken" Text :> Capture "id" Int :> Delete '[JSON] NoContent
    )
  :<|> Raw

type APIWithSwagger = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> API

apiSwagger :: Swagger
apiSwagger = toSwagger (Proxy :: Proxy API)

apiServer :: AppState -> Server API
apiServer appState =
  (     handlerDmpIndex appState
  :<|>  handlerDmpPost appState
  :<|>  handlerDmpGet appState
  :<|>  handlerDmpPut appState
  :<|>  handlerDmpDelete appState
  )
  :<|>  Tagged handlerNotFound

server :: AppState -> Server APIWithSwagger
server appState = swaggerSchemaUIServer apiSwagger :<|> apiServer appState

customCorsPolicy :: [Origin] -> CorsResourcePolicy
customCorsPolicy origins = simpleCorsResourcePolicy
  { corsOrigins = Just (origins, True)
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsMethods = ["GET", "POST", "OPTIONS", "PUT", "DELETE"]
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

app :: AppState -> [Origin] -> Application
app appState origins = cors (const $ Just $ customCorsPolicy origins) $ serve (Proxy :: Proxy APIWithSwagger) (server appState)

lookupEnvInt :: String -> Int -> IO Int
lookupEnvInt name def = do
  maybeStr <- lookupEnv name
  case maybeStr of
    Just str -> case readMaybe str of
      Just int -> return int
      Nothing -> do
        print ("Environment variable " ++ name ++ " is not a valid Int.")
        return def
    Nothing -> return def

lookupEnvHosts :: String -> [Origin] -> IO [Origin]
lookupEnvHosts name def = do
  maybeStr <- lookupEnv name
  case maybeStr of
    Just str -> return (map DS.fromString (splitOn "," str))
    Nothing -> return def

lookupDbConnectInfo :: IO ConnectInfo
lookupDbConnectInfo = do
  host <- lookupEnv "LAJI_DMP_DATABASE_HOST" <&> Data.Maybe.fromMaybe "localhost"
  port <- lookupEnvInt "LAJI_DMP_DATABASE_PORT" 5432
  user <- lookupEnv "LAJI_DMP_DATABASE_USER" <&> Data.Maybe.fromMaybe "dev"
  pwd <- lookupEnv "LAJI_DMP_DATABASE_PWD" <&> Data.Maybe.fromMaybe "1234"
  db <- lookupEnv "LAJI_DMP_DATABASE_DB" <&> Data.Maybe.fromMaybe "dmp"
  return defaultConnectInfo { connectHost = host, connectPort = fromIntegral port, connectUser = user, connectPassword = pwd, connectDatabase = db }

main :: IO ()
main = do
  origins <- lookupEnvHosts "ALLOWED_HOSTS" ["http://localhost:8000", "chrome-extension://dabkfpeebkikmgdianbkchblbdibbfhl"]
  print (show origins :: String)
  connectInfo <- lookupDbConnectInfo
  conn <- connect connectInfo
  hostPort <- lookupEnvInt "LAJI_DMP_PORT" 4000
  _ <- runMigration conn defaultOptions MigrationInitialization
  _ <- runMigration conn defaultOptions $ MigrationDirectory "./migrations"

  personCache <- newTVarIO HM.empty
  lajiApiConfig <- lookupLajiApiConfig
  httpConfig <- createHttpConfig lajiApiConfig
  let appState = AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig, appLajiApiConfig = lajiApiConfig }
  print ("Hosting on port " ++ show hostPort :: String)
  run hostPort (app appState origins)

