module Wizard.Integration.Http.Admin.RequestMapper where

import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)
import Shared.Common.Model.Http.HttpRequest
import Shared.Common.Util.String
import Wizard.Model.App.App

toRetrieveJwtPublicKeysRequest :: App -> HttpRequest
toRetrieveJwtPublicKeysRequest app =
  HttpRequest
    { requestMethod = "GET"
    , requestUrl = f' "%s/open-id-providers/00000000-0000-0000-0000-000000000000/discovery/v2.0/keys" [fromMaybe "" app.adminServerUrl]
    , requestHeaders = M.fromList [("Content-Type", "application/json")]
    , requestBody = ""
    , multipart = Nothing
    }
