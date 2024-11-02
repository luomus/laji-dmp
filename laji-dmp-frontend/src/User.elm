module User exposing (..)
import Array exposing (Array)
import Json.Decode exposing (Decoder)
import Json.Decode exposing (string)
import Json.Decode exposing (succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode exposing (array)
import Http
import Json.Encode

type PersonRole = Admin | SomethingElse

type alias PersonResponse =
  { id: String
  , fullName: String
  , role: Array PersonRole
  }

type LoginSession
  = NotLoggedIn
  | LoadingPerson String
  | DeletingToken String
  | LoggedIn String PersonResponse

encodeLogin : LoginSession -> Json.Encode.Value
encodeLogin loginSession =
  case loginSession of
    LoggedIn token personResponse ->
      Json.Encode.string token
    LoadingPerson token ->
      Json.Encode.string token
    DeletingToken token ->
      Json.Encode.string token
    NotLoggedIn ->
      Json.Encode.null

decodeLogin : Json.Decode.Value -> Maybe String
decodeLogin value =
  case Json.Decode.decodeValue Json.Decode.string value of
    Ok str -> Just str
    Err _ -> Nothing

roleDecoder : Decoder PersonRole
roleDecoder =
  Json.Decode.map (
    \r -> case r of
      "MA.admin" -> Admin
      _ -> SomethingElse
  ) string

personDecoder : Decoder PersonResponse
personDecoder =
  succeed PersonResponse
    |> required "id" string
    |> required "fullName" string
    |> required "role" (array roleDecoder)

getPerson : String -> (Result Http.Error PersonResponse -> msg) -> Cmd msg
getPerson token msg =
  Http.get
    { url = "https://dev.laji.fi/api/person/" ++ token
    , expect = Http.expectJson msg personDecoder
    }

deleteToken : String -> (Result Http.Error String -> msg) -> Cmd msg
deleteToken token msg =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = "https://dev.laji.fi/api/person-token/" ++ token
    , body = Http.emptyBody
    , expect = Http.expectString msg
    , timeout = Nothing
    , tracker = Nothing
    }
