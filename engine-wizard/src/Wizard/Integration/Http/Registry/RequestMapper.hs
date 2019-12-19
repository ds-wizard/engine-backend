module Wizard.Integration.Http.Registry.RequestMapper
  ( toRetrievePackagesRequest
  , toRetrievePackageBundleByIdRequest
  ) where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Constant.Api
import Wizard.LensesConfig
import Wizard.Model.Config.AppConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Statistics.InstanceStatistics

toRetrievePackagesRequest :: AppConfigRegistry -> InstanceStatistics -> HttpRequest
toRetrievePackagesRequest registryConfig iStat =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = registryConfig ^. url ++ "/packages"
    , _httpRequestRequestHeaders =
        M.fromList
          [ (authorizationHeaderName, "Bearer " ++ registryConfig ^. token)
          , (xUserCountHeaderName, show $ iStat ^. userCount)
          , (xPkgCountHeaderName, show $ iStat ^. pkgCount)
          , (xQtnCountHeaderName, show $ iStat ^. qtnCount)
          ]
    , _httpRequestRequestBody = ""
    }

toRetrievePackageBundleByIdRequest :: AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest registryConfig pkgId =
  HttpRequest
    { _httpRequestRequestMethod = "GET"
    , _httpRequestRequestUrl = registryConfig ^. url ++ "/packages/" ++ pkgId ++ "/bundle"
    , _httpRequestRequestHeaders = M.fromList [(authorizationHeaderName, "Bearer " ++ registryConfig ^. token)]
    , _httpRequestRequestBody = ""
    }
