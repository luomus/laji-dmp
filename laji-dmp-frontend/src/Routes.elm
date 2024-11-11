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
  | DmpInfoRoute String
  | DmpEditRoute String
  | DmpNewRoute

dmpRouteParser : Parser (DmpSubRoute -> a) a
dmpRouteParser = oneOf
  [ map DmpIndexRoute (s "dmp")
  , map DmpNewRoute (s "dmp" </> s "new")
  , map DmpInfoRoute (s "dmp" </> string)
  , map DmpEditRoute (s "dmp" </> string </> s "edit")
  ]

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map FrontRoute top
    , map DmpRoute dmpRouteParser
    , map LoginRoute (s "login" <?> Query.string "access_token" <?> Query.string "next")
    ]

fromUrl : Url -> Maybe Route
fromUrl url =
  parse routeParser url
