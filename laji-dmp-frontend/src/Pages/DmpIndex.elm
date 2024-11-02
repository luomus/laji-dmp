module Pages.DmpIndex exposing (..)

import Http
import Browser
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (text)
import Views.Navigation exposing (navigation)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (Html)
import Html exposing (div)
import Config exposing (config)
import Json.Decode
import Json.Decode.Pipeline
import Array
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd

type Model = Loading | Error | DmpList DmpListResponse

type Msg = GotDmpListResponse (Result Http.Error DmpListResponse)

type alias DmpListResponseElement =
  { id: Int
  , testField: String
  }

type alias DmpListResponse = Array.Array DmpListResponseElement

dmpListElementDecoder : Json.Decode.Decoder DmpListResponseElement
dmpListElementDecoder =
  Json.Decode.succeed DmpListResponseElement
    |> Json.Decode.Pipeline.required "plan_id" Json.Decode.int
    |> Json.Decode.Pipeline.required "test_field" Json.Decode.string

dmpListDecoder : Json.Decode.Decoder DmpListResponse
dmpListDecoder = Json.Decode.array dmpListElementDecoder

getDmpList : (Result Http.Error DmpListResponse -> msg) -> Cmd msg
getDmpList msg =
  Http.get
    { url = config.dmpApiUrl ++ "dmp"
    , expect = Http.expectJson msg dmpListDecoder
    }

init : ( Model, Cmd Msg )
init = (Loading, getDmpList GotDmpListResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDmpListResponse res ->
      case res of
        Ok dmpList -> (DmpList dmpList, Cmd.none)
        Err e ->
          let _ = Debug.log "Error loading DMP list" e
          in (Error, Cmd.none)

dmpListElementView : DmpListResponseElement -> Html msg
dmpListElementView elem =
  li [] [ a [href <| "dmp/" ++ String.fromInt elem.id] [text elem.testField] ]

view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Dmp Index View"
  , body = div []
    <| case model of
      Error -> [ text "Failed to load the list of DMPs." ]
      Loading -> [ text "Loading the list of DMPs..." ]
      DmpList dmpList -> 
        [
          ul []
            <| Array.toList <| Array.map dmpListElementView dmpList
          , a [href "/dmp/new"] [text "New DMP"]
        ]
  }
