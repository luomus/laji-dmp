module Organization exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Array exposing (Array)
import Config exposing (Config)
import Http

type alias PagedResponse a =
  { total: Int
  , results: Array a
  , currentPage: Int
  , pageSize: Int
  , lastPage: Int
  }

type alias Organization =
  { id: String
  , organizationLevel1: Maybe String
  , organizationLevel2: Maybe String
  , organizationLevel3: Maybe String
  , organizationLevel4: Maybe String
  }

type alias OrganizationLookup =
  Dict String Organization

organizationDecoder : D.Decoder Organization
organizationDecoder =
  D.succeed Organization
    |> D.required "id" D.string
    |> D.optional "organizationLevel1" (D.nullable D.string) Nothing
    |> D.optional "organizationLevel2" (D.nullable D.string) Nothing
    |> D.optional "organizationLevel3" (D.nullable D.string) Nothing
    |> D.optional "organizationLevel4" (D.nullable D.string) Nothing

organizationsDecoder : D.Decoder (Array Organization)
organizationsDecoder = D.array organizationDecoder

pagedOrganizationsDecoder : D.Decoder (PagedResponse Organization)
pagedOrganizationsDecoder =
  D.succeed PagedResponse
    |> D.required "total" D.int
    |> D.required "results" organizationsDecoder
    |> D.required "currentPage" D.int
    |> D.required "pageSize" D.int
    |> D.required "lastPage" D.int

getOrganizations : Config -> (Result Http.Error (PagedResponse Organization) -> msg) -> Cmd msg
getOrganizations cfg msg =
  Http.request
    { method = "GET"
    , headers = [Http.header "API-Version" "1", Http.header "accept" "application/json"]
    , url = cfg.lajiApiBase ++ "/organizations?selectedFields=id,organizationLevel1,organizationLevel2,organizationLevel3,organizationLevel4&page=1&pageSize=2000"
    , body = Http.emptyBody
    , expect = Http.expectJson msg pagedOrganizationsDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

type alias OrgLookup = Dict String Organization

updateOrganizationsDict : PagedResponse Organization -> OrgLookup -> Dict String Organization
updateOrganizationsDict res dict =
  Array.foldl (\elem d -> Dict.insert elem.id elem d) dict res.results

