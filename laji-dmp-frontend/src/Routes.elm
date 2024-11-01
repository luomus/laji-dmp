module Routes exposing (..)

import Url.Parser exposing (Parser, oneOf, s, map, top, (</>), (<?>), string, parse)
import Url exposing (Url)
import Url.Parser.Query as Query

type Route
  = FrontRoute
  | DmpIndexRoute
  | DmpInfoRoute String
  | DmpEditRoute String
  | DmpNewRoute
  | LoginRoute (Maybe String) (Maybe String)

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map FrontRoute top
    , map DmpIndexRoute (s "dmp")
    , map DmpNewRoute (s "dmp" </> s "new")
    , map DmpInfoRoute (s "dmp" </> string)
    , map DmpEditRoute (s "dmp" </> string </> s "edit")
    , map LoginRoute (s "login" <?> Query.string "access_token" <?> Query.string "next")
    ]

fromUrl : Url -> Maybe Route
fromUrl url =
  parse routeParser url
