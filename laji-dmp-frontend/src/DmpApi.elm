module DmpApi exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Array
import Http
import Config exposing (config)
import Json.Encode

type alias DataManagementPlan =
  { id: Maybe Int
  , testField: String
  }

type alias DmpList = Array.Array DataManagementPlan

dmpDecoder : Json.Decode.Decoder DataManagementPlan
dmpDecoder =
  Json.Decode.succeed DataManagementPlan
    |> Json.Decode.Pipeline.optional "plan_id" (Json.Decode.map (\x -> Just x) Json.Decode.int) Nothing
    |> Json.Decode.Pipeline.required "test_field" Json.Decode.string

dmpListDecoder : Json.Decode.Decoder DmpList
dmpListDecoder = Json.Decode.array dmpDecoder

encodeDmp : DataManagementPlan -> Json.Encode.Value
encodeDmp dmp = Json.Encode.object
  [ ( "plan_id"
    , case dmp.id of
      Just id -> Json.Encode.int id
      Nothing -> Json.Encode.null
    )
  , ( "test_field", Json.Encode.string dmp.testField )
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
    { method = "PUT"
    , headers = []
    , url = config.dmpApiUrl ++ "dmp"
    , body = Http.jsonBody <| encodeDmp dmp
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }
