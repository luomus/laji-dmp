{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Errors (JSONErr) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import Servant
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Data.List.NonEmpty (NonEmpty(..))

data DecodeError = DecodeError
  { code    :: T.Text
  , path    :: [T.Text]
  , message :: T.Text
  } deriving (Show, Generic)

instance A.ToJSON DecodeError

jsonPathSegments :: A.JSONPath -> [T.Text]
jsonPathSegments = map toSeg
  where
    toSeg (A.Key k)   = K.toText k
    toSeg (A.Index i) = T.pack (show i)

data JSONErr
instance Accept JSONErr where
  contentTypes _ =
    ("application" // "json")
      :| [ "application" // "json" /: ("charset","utf-8")
         , "application" // "problem+json"
         ]

eitherDecodeWithPath
  :: A.FromJSON a
  => BL.ByteString
  -> Either (A.JSONPath, String) a
eitherDecodeWithPath bs =
  case A.eitherDecode bs of
    Left err -> Left ([], err)
    Right v  -> case A.ifromJSON v of
      A.ISuccess a -> Right a
      A.IError path msg -> Left (path, msg)

instance (FromJSON a) => MimeUnrender JSONErr a where
  mimeUnrender _ bs =
    case eitherDecodeWithPath bs of
      Right a -> Right a
      Left (path, msg) ->
        let de = DecodeError
                  { code = "INVALID_JSON_BODY"
                  , path = jsonPathSegments path
                  , message = T.pack msg
                  }
        in Left (BL8.unpack (A.encode de))

