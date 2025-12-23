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
import Array
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd
import Html.Attributes exposing (class)
import Html exposing (table)
import Html exposing (tr)
import Html exposing (th)
import Html exposing (td)
import Html exposing (h5)
import User exposing (LoginSession)
import DmpApi exposing (getDmpList)
import Models exposing (Dmp)
import Config exposing (Config)
import Dict exposing (Dict)
import Organization exposing (Organization)
import Utils exposing (showOrgName)
import Organization exposing (OrgLookup)
import Set exposing (Set)
import Html exposing (label)
import Html exposing (input)
import Html.Attributes exposing (value)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onInput)
import Html.Attributes exposing (type_)
import User exposing (LoginSession(..))
import Html exposing (h4)

type alias DmpListPartition = 
  { hasAccess: List Dmp
  , noAccess: List Dmp
  }

type DmpListState = Loading | Error String | DmpList DmpListPartition

type alias Model =
  { dmpList: DmpListState
  , session: LoginSession
  , orgFilter: String
  }

type Msg
  = GotDmpListResponse (Result Http.Error (Array.Array Dmp))
  | OnModifyOrgFilter String

init : Config -> LoginSession -> ( Model, Cmd Msg )
init cfg session =
  ( { dmpList = Loading, session = session, orgFilter = "" }
    , getDmpList cfg GotDmpListResponse
  )

partitionDmps : Array.Array Dmp -> LoginSession -> DmpListPartition
partitionDmps dmps session =
  let
    hasAccess : Dmp -> Bool
    hasAccess dmp = case session of
      LoggedIn token person ->
        Array.length (Array.filter (\org -> org == dmp.dmpOrgId) person.organisation) > 0
      _ -> False
    dmpsWithAccess = Array.filter hasAccess dmps
    dmpsWithoutAccess = Array.filter (\dmp -> not <| hasAccess dmp) dmps
    dmpsWithoutAccessSorted = List.sortBy .dmpOrgId (Array.toList dmpsWithoutAccess)
  in { hasAccess = Array.toList dmpsWithAccess, noAccess = dmpsWithoutAccessSorted }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDmpListResponse res ->
      case res of
        Ok dmpList -> ({ model | dmpList = DmpList (partitionDmps dmpList model.session) }, Cmd.none)
        Err e ->
          ({ model | dmpList = Error "Failed to load DMP list response" }, Cmd.none )
    OnModifyOrgFilter str ->
      ( { model | orgFilter = str }
      , Cmd.none
      )

dmpElementView : Dmp -> Bool -> OrgLookup -> Html msg
dmpElementView elem hasAccess orgs =
  case elem.dmpId of
    Just id -> div [ class "dmp-index dmp-element" ]
      [ a [ href <| "dmp/" ++ String.fromInt id, class "dmp-index-dmp-box" ]
        [ h5 [] [ text <| elem.dmpTitle ]
        , div [] [ text <| "Organisaatio: " ++ showOrgName elem orgs ]
        , div [] [ text <| String.fromInt (Array.length elem.dmpDatasets) ++ " aineistoa" ]
        ]
      , if hasAccess
        then div [] [a [href <| "/dmp/" ++ (String.fromInt id) ++"/edit", class "btn"] [text "Muokkaa"]]
        else text ""
      ]
    Nothing -> li [] [text "Virhe: DMP:n tunniste puuttuu"]

dmpTableView : List Dmp -> Bool -> String -> OrgLookup -> Html Msg
dmpTableView dmpList hasAccess filterStr orgs
  = div []
    <| List.map (\a -> dmpElementView a hasAccess orgs)
    <| List.filter
      ( \dmp 
        -> String.contains (String.toLower filterStr)
        <| String.toLower
        <| showOrgName dmp orgs
      ) dmpList

view : Model -> OrgLookup -> { title : String, body : Html Msg }
view model orgs =
  { title = "DMP-luettelo"
  , body = div [class "dmp-index"]
    [ label []
      [ text "Suodata organisaation nimellä: "
      , input 
        [ value model.orgFilter
        , disabled <| case model.dmpList of
            DmpList _ -> False
            _ -> True
        , onInput <| OnModifyOrgFilter
        , type_ "text"
        ]
        []
      ]
    , div [] <| case model.dmpList of
      Error err -> [ text <| "Virhe: " ++ err ]
      Loading -> [ text "Ladataan DMP-luetteloa..." ]
      DmpList dmpList -> 
        [ h4 [] [ text "Oman organisaation DMP:t" ]
        , dmpTableView dmpList.hasAccess True model.orgFilter orgs
        , h4 [] [ text "Muiden organisaatioiden DMP:t" ]
        , dmpTableView dmpList.noAccess False model.orgFilter orgs
        ]
    , div [] <| case model.session of
      User.LoggedIn personToken personResponse ->
        if Array.isEmpty personResponse.organisation
          then [ text "Huomio: luodaksesi uuden DMP:n käyttäjätunnuksesi tulee kuulua johonkin organisaatioon." ]
          else [ a [href "/dmp/new", class "btn btn-primary"] [text "+ Uusi DMP"] ]
      _ -> []
    ]
  }
