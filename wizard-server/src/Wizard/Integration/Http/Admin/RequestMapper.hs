module Wizard.Integration.Http.Admin.RequestMapper where

import qualified Data.Map.Strict as M

import Shared.Common.Util.String
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest

toRetrieveJwtPublicKeysRequest :: ServerConfigAdmin -> HttpRequest
toRetrieveJwtPublicKeysRequest serverConfig =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = f' "%s/open-id/%s/discovery/v2.0/keys" [serverConfig.url, "default"]
    , requestHeaders = M.fromList [("Content-Type", "application/json")]
    , requestBody = ""
    , multipart = Nothing
    }
