module Config exposing (..)

type alias Config =
  { dmpApiUrl: String
  , lajiApiUrl: String
  }

config : Config
config =
  { dmpApiUrl = "http://localhost:4000"
  , lajiApiUrl = ""
  }
