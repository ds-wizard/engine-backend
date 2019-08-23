module Integration.Http.Registry.RequestMapper
  ( toRetrievePackagesRequest
  , toRetrievePackageBundleByIdRequest
  ) where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Prelude hiding (lookup)

import Constant.Api
import LensesConfig
import Model.Config.AppConfig
import Model.Http.HttpRequest
import Model.Statistics.InstanceStatistics

toRetrievePackagesRequest :: AppConfigRegistry -> InstanceStatistics -> HttpRequest
toRetrievePackagesRequest registryConfig iStat =
  HttpRequest
  { _httpRequestRequestMethod = "GET"
  , _httpRequestRequestUrl = registryConfig ^. url ++ "/packages"
  , _httpRequestRequestHeaders =
      M.fromList
        [ (authorizationHeaderName, "Bearer " ++ registryConfig ^. token)
        , (xDswUserCountHeaderName, show $ iStat ^. userCount)
        , (xDswPkgCountHeaderName, show $ iStat ^. pkgCount)
        , (xDswQtnCountHeaderName, show $ iStat ^. qtnCount)
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
