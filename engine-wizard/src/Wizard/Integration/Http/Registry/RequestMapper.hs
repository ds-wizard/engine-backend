module Wizard.Integration.Http.Registry.RequestMapper
  ( toRetrievePackagesRequest
  , toRetrievePackageBundleByIdRequest
  ) where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Constant.Api
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Statistics.InstanceStatistics

toRetrievePackagesRequest :: ServerConfigRegistry -> AppConfigRegistry -> InstanceStatistics -> HttpRequest
toRetrievePackagesRequest serverConfig appConfig iStat =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = serverConfig ^. url ++ "/packages"
    , _httpRequestRequestHeaders =
        M.fromList
          [ (authorizationHeaderName, "Bearer " ++ appConfig ^. token)
          , (xUserCountHeaderName, show $ iStat ^. userCount)
          , (xPkgCountHeaderName, show $ iStat ^. pkgCount)
          , (xQtnCountHeaderName, show $ iStat ^. qtnCount)
          ]
    , _httpRequestRequestBody = ""
    }

toRetrievePackageBundleByIdRequest :: ServerConfigRegistry -> AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest serverConfig appConfig pkgId =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = serverConfig ^. url ++ "/packages/" ++ pkgId ++ "/bundle"
    , _httpRequestRequestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig ^. token)]
    , _httpRequestRequestBody = ""
    }
