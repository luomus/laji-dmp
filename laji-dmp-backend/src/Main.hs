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
import qualified Data.Text.Lazy as Text.Lazy

data AppState = AppState
  { appDbConnection :: Connection
  , appPersonCache :: TVar (HM.HashMap Text (CacheElement Person))
  , appHttpConfig :: HttpConfig
  }

checkUserRights :: Text -> Text -> TVar (HM.HashMap Text (CacheElement Person)) -> HttpConfig -> IO Bool
checkUserRights personToken orgId personCache httpConfig =
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
      maybePerson <- getPerson personToken personCache httpConfig
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
handlerDmpPost (AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }) personToken dmp = do
  userHasRights <- liftIO $ checkUserRights personToken (Models.dmpOrgId dmp) personCache httpConfig
  if userHasRights
    then do
      liftIO $ Queries.insertDataManagementPlan conn dmp
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
handlerDmpPut (AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }) personToken dmpId dmp =
  let 
    idMatch = case Models.dmpId dmp of
      Just i -> i == dmpId
      Nothing -> True
  in do
    orgMatch <- do
      oldPlanRes <- liftIO $ Queries.getDataManagementPlan conn dmpId
      case oldPlanRes of
        Right oldPlan ->
          return $ Models.dmpOrgId (head oldPlan) == Models.dmpOrgId dmp
        Left err -> throwError err500 { errBody = encodeUtf8 $ Text.Lazy.pack err }
    userHasRights <- liftIO $ checkUserRights personToken (Models.dmpOrgId dmp) personCache httpConfig
    if idMatch
      then if orgMatch
        then if userHasRights
          then do
            liftIO $ Queries.updateDataManagementPlan conn dmpId dmp
            return NoContent
          else do
            throwError err401 { errBody = "User doesn't have rights for this organization." }
        else do
          throwError err403 { errBody = "DMPs can't be transferred between organizations." }
      else do
        throwError err403 { errBody = "Id mismatch." }

handlerDmpDelete :: AppState -> Text -> Int -> Handler NoContent
handlerDmpDelete (AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }) personToken dmpId = do
  userHasRights <- liftIO $ checkUserRights personToken "" personCache httpConfig
  if userHasRights
    then do
      liftIO $ Queries.deleteDataManagementPlan conn dmpId
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

customCorsPolicy :: CorsResourcePolicy
customCorsPolicy = simpleCorsResourcePolicy
  { corsOrigins = Just (["http://localhost:8000", "chrome-extension://dabkfpeebkikmgdianbkchblbdibbfhl"], True)
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsMethods = ["GET", "POST", "OPTIONS", "PUT", "DELETE"]
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

app :: AppState -> Application
app appState = cors (const $ Just customCorsPolicy) $ serve (Proxy :: Proxy APIWithSwagger) (server appState)

main :: IO ()
main = do
  -- dbPath <- lookupEnv "LAJI_DMP_DATABASE" <&> Data.Maybe.fromMaybe "laji-dmp.db"
  conn <- connect defaultConnectInfo {
      connectHost = "localhost",
      connectPort = 5432,
      connectUser = "dev",
      connectPassword = "1234",
      connectDatabase = "dmp"
  }

  Queries.initializeDatabase conn
  print ("Hosting on port 4000" :: String)
  personCache <- newTVarIO HM.empty
  httpConfig <- createHttpConfig
  let appState = AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }
  run 4000 (app appState)

