module Routes exposing (..)

import Url.Parser exposing (Parser, oneOf, s, map, top, (</>), (<?>), string, parse)
import Url exposing (Url)
import Url.Parser.Query as Query

type Route
  = FrontRoute
  | DmpRoute DmpSubRoute
  | LoginRoute (Maybe String) (Maybe String)

type DmpSubRoute
  = DmpIndexRoute
  | DmpNewRoute
  | DmpElementRoute DmpElementSubRoute

type DmpElementSubRoute
  = DmpInfoRoute String
  | DmpEditRoute String

dmpElementRouteParser : Parser (DmpElementSubRoute -> a) a
dmpElementRouteParser = oneOf
  [ map DmpInfoRoute (string)
  , map DmpEditRoute (string </> s "edit")
  ]

dmpRouteParser : Parser (DmpSubRoute -> a) a
dmpRouteParser = oneOf
  [ map DmpIndexRoute top
  , map DmpNewRoute (s "new")
  , map DmpElementRoute dmpElementRouteParser
  ]

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map FrontRoute top
    , s "dmp" </> (map DmpRoute dmpRouteParser)
    , map LoginRoute (s "login" <?> Query.string "access_token" <?> Query.string "next")
    ]

fromUrl : Url -> Maybe Route
fromUrl url =
  parse routeParser url
