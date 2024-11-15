module DmpApi exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Array
import Http
import Config exposing (config)
import Json.Encode as E
import Json.Decode as D
import Json.Decode.Pipeline as DP

type DataAccess = Open | Shared | Closed
type PersonalData = Yes | No | Unknown

type alias Host =
  { backupFrequency: Maybe String
  , geoLocation: Maybe String
  }

type alias Distribution =
  { dataAccess: DataAccess
  , accessUrl: Maybe String
  , host: Maybe Host
  }

type alias Dataset =
  { title: String
  , personalData: PersonalData
  , distributions: Array.Array Distribution
  }

type alias DataManagementPlan =
  { id: Maybe Int
  , datasets: Array.Array Dataset
  }

type alias DmpList = Array.Array DataManagementPlan

hostDecoder : Json.Decode.Decoder Host
hostDecoder = Json.Decode.succeed Host
  |> DP.optional "backup_frequency" (D.nullable D.string) Nothing
  |> DP.optional "geo_location" (D.nullable D.string) Nothing

dataAccessDecoder : Json.Decode.Decoder DataAccess
dataAccessDecoder =
  let
    f str = case str of
      "Open" -> Open
      "Shared" -> Shared
      _ -> Closed
  in Json.Decode.map f Json.Decode.string

distributionDecoder : Json.Decode.Decoder Distribution
distributionDecoder = Json.Decode.succeed Distribution
  |> DP.required "data_access" dataAccessDecoder
  |> DP.optional "access_url" (D.nullable D.string) Nothing
  |> DP.optional "host" (D.nullable hostDecoder) Nothing

personalDataDecoder : Json.Decode.Decoder PersonalData
personalDataDecoder =
  let
    f str = case str of
      "Yes" -> Yes
      "No" -> No
      _ -> Unknown
  in Json.Decode.map f D.string

datasetDecoder : Json.Decode.Decoder Dataset
datasetDecoder = Json.Decode.succeed Dataset
  |> DP.required "title" D.string
  |> DP.required "personal_data" personalDataDecoder
  |> DP.required "distributions" (Json.Decode.array distributionDecoder)

dmpDecoder : Json.Decode.Decoder DataManagementPlan
dmpDecoder = Json.Decode.succeed DataManagementPlan
  |> DP.optional "plan_id" (Json.Decode.map (\x -> Just x) Json.Decode.int) Nothing
  |> DP.required "datasets" (Json.Decode.array datasetDecoder)

dmpListDecoder : Json.Decode.Decoder DmpList
dmpListDecoder = Json.Decode.array dmpDecoder

encodeHost : Host -> E.Value
encodeHost host = E.object
  [ ( "backup_frequency"
    , case host.backupFrequency of
      Just bf -> E.string bf
      Nothing -> E.null
    )
  , ( "geo_location"
    , case host.geoLocation of
      Just geo -> E.string geo
      Nothing -> E.null
    )
  ]

encodeDataAccess : DataAccess -> E.Value
encodeDataAccess dataAccess = E.string
  <| case dataAccess of
    Open -> "Open"
    Shared -> "Shared"
    Closed -> "Closed"

encodeDistribution : Distribution -> E.Value
encodeDistribution distribution = E.object
  [ ( "data_access", encodeDataAccess distribution.dataAccess )
  , ( "access_url"
    , case distribution.accessUrl of
      Just url -> E.string url
      Nothing -> E.null
    )
  , ( "host"
    , case distribution.host of
      Just h -> encodeHost h
      Nothing -> E.null
    )
  ]

encodePersonalData : PersonalData -> E.Value
encodePersonalData personalData = E.string
  <| case personalData of
    Yes -> "Yes"
    No -> "No"
    Unknown -> "Unknown"

encodeDataset : Dataset -> E.Value
encodeDataset dataset = E.object
  [ ( "title", E.string dataset.title )
  , ( "personal_data", encodePersonalData dataset.personalData )
  , ( "distributions", E.array encodeDistribution dataset.distributions )
  ]

encodeDmp : DataManagementPlan -> E.Value
encodeDmp dmp = E.object
  [ ( "plan_id", case dmp.id of
      Just id -> E.int id
      Nothing -> E.null
    )
  , ( "datasets", E.array encodeDataset dmp.datasets )
  ]

getDmpList : (Result Http.Error DmpList -> msg) -> Cmd msg
getDmpList msg =
  Http.get
    { url = config.dmpApiUrl ++ "dmp"
    , expect = Http.expectJson msg dmpListDecoder
    }

getDmp : Int -> (Result Http.Error DataManagementPlan -> msg) -> Cmd msg
getDmp id msg =
  Http.get
    { url = config.dmpApiUrl ++ "dmp/" ++ String.fromInt id
    , expect = Http.expectJson msg dmpDecoder
    }

newDmp : DataManagementPlan -> (Result Http.Error String -> msg) -> Cmd msg
newDmp dmp msg =
  Http.request
    { method = "POST"
    , headers = []
    , url = config.dmpApiUrl ++ "dmp"
    , body = Http.jsonBody <| encodeDmp dmp
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }

editDmp : String -> DataManagementPlan -> (Result Http.Error String -> msg) -> Cmd msg
editDmp id dmp msg =
  Http.request
    { method = "PUT"
    , headers = []
    , url = config.dmpApiUrl ++ "dmp/" ++ id
    , body = Http.jsonBody <| encodeDmp dmp
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }

deleteDmp : String -> (Result Http.Error String -> msg) -> Cmd msg
deleteDmp id msg =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = config.dmpApiUrl ++ "dmp/" ++ id
    , body = Http.jsonBody <| E.null
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }
