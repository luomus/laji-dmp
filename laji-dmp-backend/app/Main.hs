{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant ( Get
               , Proxy(..)
               , type (:>)
               , type (:<|>)
               , (:<|>)(..), Put, Capture, JSON, Post, ReqBody, Raw, NoContent (..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai (Application, responseLBS, Request (pathInfo))
import Network.HTTP.Types (status404)
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple
import Servant (Tagged(Tagged), NoContent)

import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

import Database
import System.Environment (lookupEnv)
import qualified Data.Maybe
import Data.Functor ((<&>))

handlerDmpIndex :: Connection -> Handler [DataManagementPlan]
handlerDmpIndex conn = do
  liftIO $ getDataManagementPlans conn

handlerDmpPut :: Connection -> DataManagementPlan -> Handler NoContent
handlerDmpPut conn plan = do
  liftIO $ createDataManagementPlan conn plan
  return NoContent

handlerDmpGet :: Connection -> Int -> Handler DataManagementPlan
handlerDmpGet conn id = do
  liftIO $ head <$> getDataManagementPlan conn id

handlerDmpPost :: Connection -> Int -> DataManagementPlan -> Handler NoContent
handlerDmpPost conn plan_id plan = do
  liftIO $ updateDataManagementPlan conn plan_id plan
  return NoContent

handlerNotFound :: Application
handlerNotFound _ respond = respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Route Not Found"

type API =
  "dmp" :>
    (     Get '[JSON] [DataManagementPlan]
    :<|>  ReqBody '[JSON] DataManagementPlan :> Put '[JSON] NoContent
    :<|>  Capture "id" Int :> Get '[JSON] DataManagementPlan
    :<|>  Capture "id" Int :> ReqBody '[JSON] DataManagementPlan :> Post '[JSON] NoContent
    )
  :<|>  Raw

type APIWithSwagger = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> API

apiSwagger :: Swagger
apiSwagger = toSwagger (Proxy :: Proxy API)

apiServer :: Connection -> Server API
apiServer conn =
  (     handlerDmpIndex conn
  :<|>  handlerDmpPut   conn
  :<|>  handlerDmpGet   conn
  :<|>  handlerDmpPost  conn
  )
  :<|>  Tagged handlerNotFound

server :: Connection -> Server APIWithSwagger
server conn = swaggerSchemaUIServer apiSwagger :<|> apiServer conn

customCorsPolicy :: CorsResourcePolicy
customCorsPolicy = simpleCorsResourcePolicy
  { corsOrigins = Just (["http://localhost:8000"], True)
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsMethods = ["GET", "POST", "OPTIONS", "PUT", "DELETE"]
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

app :: Connection -> Application
app conn = cors (const $ Just customCorsPolicy) $ serve (Proxy :: Proxy APIWithSwagger) (server conn)

main :: IO ()
main = do
  dbPath <- lookupEnv "LAJI_DMP_DATABASE" <&> Data.Maybe.fromMaybe "laji-dmp.db"
  conn <- open dbPath
  createDataManagementPlanTable conn
  print ("Hosting on port 4000" :: String)
  run 4000 (app conn)

