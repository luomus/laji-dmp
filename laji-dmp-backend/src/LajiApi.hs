{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LajiApi (getPerson, role, Person, organisation, organisationAdmin, CacheElement, createHttpConfig, LajiApiConfig, lookupLajiApiConfig) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Network.TLS
import Network.Connection
import Data.Default.Class (def)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Client (newManager)
import qualified Data.ByteString.Char8 as BS
import GHC.Generics (Generic)
import Data.Text
import Control.Exception (try, SomeException)
import qualified Data.X509.CertificateStore as X509
import Data.X509.CertificateStore (CertificateStore)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import System.Environment (lookupEnv)
import Data.Functor ((<&>))
import Data.Text as T
import qualified Data.Maybe

data CacheElement t = CacheElement
  { cacheElementLastUpdate :: UTCTime
  , cacheElementValue :: t
  }

data Person = Person
  { id :: String
  , fullName :: String
  , role :: [String]
  , organisation :: Maybe [String]
  , organisationAdmin :: Maybe [String]
  } deriving (Show, Generic)
instance FromJSON Person

data LajiApiConfig = LajiApiConfig
  { lajiApiHostName :: String
  , lajiApiAccessToken :: String
  }

lookupLajiApiConfig :: IO LajiApiConfig
lookupLajiApiConfig = do
  hostName <- lookupEnv "LAJI_API_HOSTNAME" <&> Data.Maybe.fromMaybe "dev.laji.fi"
  accessToken <- lookupEnv "LAJI_API_ACCESS_TOKEN" <&> Data.Maybe.fromMaybe "2fa73578d9328ff47705a34bb3073a000953c8b1569205c798e540b80e9aea10"
  return LajiApiConfig { lajiApiHostName = hostName, lajiApiAccessToken = accessToken}

defaultClientParams :: (HostName, BS.ByteString) -> CertificateStore -> ClientParams
defaultClientParams (hostName, serviceBlob) caStore = (defaultParamsClient hostName serviceBlob)
    { clientUseMaxFragmentLength      = Nothing
    , clientServerIdentification      = (hostName, serviceBlob)
    , clientUseServerNameIndication   = True
    , clientWantSessionResume         = Nothing
    , clientShared                    = def
      { sharedCAStore = caStore
      }
    , clientHooks = def
    , clientSupported                 = def
      { supportedExtendedMainSecret = NoEMS
      }
    , clientDebug                     = def
    }

createHttpConfig :: LajiApiConfig -> IO HttpConfig
createHttpConfig cfg = do
  maybeCaStore <- X509.readCertificateStore "letsencrypt-ca.pem"
  let caStore = maybe (error "Failed to load CA certificates") Prelude.id maybeCaStore
  let tlsSettings = TLSSettings $ defaultClientParams (lajiApiHostName cfg, "") caStore
  let managerSettings = mkManagerSettings tlsSettings Nothing
  manager <- newManager managerSettings
  let customHttpConfig = defaultHttpConfig { httpConfigAltManager = Just manager }
  return customHttpConfig

getPerson :: Text -> TVar (HM.HashMap Text (CacheElement Person)) -> LajiApiConfig -> HttpConfig-> IO (Maybe Person)
getPerson personToken personCache lajiApiConfig httpConfig =
  let
    makeRequest = runReq httpConfig $ req
      GET
      (https (T.pack $ lajiApiHostName lajiApiConfig) /: "api" /: "person" /: personToken)
      NoReqBody
      jsonResponse
      ("access_token" =: T.pack (lajiApiAccessToken lajiApiConfig))
    updateCache :: UTCTime -> IO (Maybe Person)
    updateCache currentTime = do
      result <- try makeRequest :: IO (Either SomeException (JsonResponse (Maybe Person)))
      case result of
        Left err -> do
          return Nothing
        Right res -> do
          let maybePerson = responseBody res :: Maybe Person
          case maybePerson of
            Just person ->
              atomically $ modifyTVar' personCache (
                HM.insert personToken CacheElement
                  { cacheElementLastUpdate = currentTime
                  , cacheElementValue = person
                  }
              )
            _ -> return ()
          return maybePerson
  in do
    cache <- readTVarIO personCache
    currentTime <- getCurrentTime
    let maybeCacheElement = HM.lookup personToken cache
    case maybeCacheElement of
      Just cacheElement ->
        if currentTime < addUTCTime (24 * 3600) (cacheElementLastUpdate cacheElement)
          then return $ Just $ cacheElementValue cacheElement
          else updateCache currentTime
      Nothing -> updateCache currentTime

