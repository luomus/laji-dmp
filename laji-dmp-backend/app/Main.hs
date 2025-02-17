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
import Database.SQLite.Simple
import Servant
import qualified Data.HashMap.Strict as HM

import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

import Database
import LajiApi
import System.Environment (lookupEnv)
import qualified Data.Maybe
import Data.Functor ((<&>))
import Data.Text (Text, unpack, pack)
import Control.Concurrent.STM
import Network.HTTP.Req (HttpConfig)

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

handlerDmpIndex :: AppState -> Handler [DataManagementPlan]
handlerDmpIndex (AppState { appDbConnection = conn }) = do
  liftIO $ getDataManagementPlans conn

handlerDmpPost :: AppState -> Text -> DataManagementPlan -> Handler NoContent
handlerDmpPost (AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }) personToken plan = do
  userHasRights <- liftIO $ checkUserRights personToken (pack $ org_id plan) personCache httpConfig
  if userHasRights
    then do
      liftIO $ createDataManagementPlan conn plan
      return NoContent
    else do
      throwError err403 { errBody = "User doesn't have rights for this organization." }

handlerDmpGet :: AppState -> Int -> Handler DataManagementPlan
handlerDmpGet (AppState { appDbConnection = conn }) id = do
  maybeDmp <- liftIO $ Data.Maybe.listToMaybe <$> getDataManagementPlan conn id
  case maybeDmp of
    Just dmp -> return dmp
    Nothing -> throwError err404 { errBody = "No such DMP was found." }

handlerDmpPut :: AppState -> Text -> Int -> DataManagementPlan -> Handler NoContent
handlerDmpPut (AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }) personToken planId plan =
  let 
    idMatch = case plan_id plan of
      Just i -> i == planId
      Nothing -> False
  in do
    orgMatch <- do
      oldPlan <- liftIO $ getDataManagementPlan conn planId
      return $ org_id (head oldPlan) == org_id plan
    userHasRights <- liftIO $ checkUserRights personToken (pack $ org_id plan) personCache httpConfig
    if idMatch
      then if orgMatch
        then if userHasRights
          then do
            liftIO $ updateDataManagementPlan conn planId plan
            return NoContent
          else do
            throwError err401 { errBody = "User doesn't have rights for this organization." }
        else do
          throwError err403 { errBody = "DMPs can't be transferred between organizations." }
      else do
        throwError err403 { errBody = "Id mismatch." }

handlerDmpDelete :: AppState -> Text -> Int -> Handler NoContent
handlerDmpDelete (AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }) personToken planId = do
  userHasRights <- liftIO $ checkUserRights personToken "" personCache httpConfig
  if userHasRights
    then do
      liftIO $ deleteDataManagementPlan conn planId
      return NoContent
    else do
      throwError err403 { errBody = "User doesn't have rights for this organization." }

handlerNotFound :: Application
handlerNotFound _ respond = respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Route Not Found"

type API =
  "dmp" :>
    (     Get '[JSON] [DataManagementPlan]
    :<|>  QueryParam' '[Required] "personToken" Text :> ReqBody '[JSON] DataManagementPlan :> Post '[JSON] NoContent
    :<|>  Capture "id" Int :> Get '[JSON] DataManagementPlan
    :<|>  QueryParam' '[Required] "personToken" Text :> Capture "id" Int :> ReqBody '[JSON] DataManagementPlan :> Put '[JSON] NoContent
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
  dbPath <- lookupEnv "LAJI_DMP_DATABASE" <&> Data.Maybe.fromMaybe "laji-dmp.db"
  conn <- open dbPath
  enableForeignKeys conn
  createDataManagementPlanTable conn
  print ("Hosting on port 4000" :: String)
  personCache <- newTVarIO HM.empty
  httpConfig <- createHttpConfig
  let appState = AppState { appDbConnection = conn, appPersonCache = personCache, appHttpConfig = httpConfig }
  run 4000 (app appState)

