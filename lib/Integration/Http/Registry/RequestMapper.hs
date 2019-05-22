module Integration.Http.Registry.RequestMapper
  ( toRetrievePackagesRequest
  , toRetrievePackageBundleByIdRequest
  ) where

import Control.Lens ((^.))
import Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Model.Config.AppConfig
import Model.Http.HttpRequest

toRetrievePackagesRequest :: AppConfigRegistry -> HttpRequest
toRetrievePackagesRequest registryConfig =
  HttpRequest
  { _httpRequestRequestMethod = "GET"
  , _httpRequestRequestUrl = registryConfig ^. url ++ "/packages"
  , _httpRequestRequestHeaders = M.fromList [("Authorization", "Bearer " ++ registryConfig ^. token)]
  , _httpRequestRequestBody = ""
  }

toRetrievePackageBundleByIdRequest :: AppConfigRegistry -> String -> HttpRequest
toRetrievePackageBundleByIdRequest registryConfig pkgId =
  HttpRequest
  { _httpRequestRequestMethod = "GET"
  , _httpRequestRequestUrl = registryConfig ^. url ++ "/packages/" ++ pkgId ++ "/bundle"
  , _httpRequestRequestHeaders = M.fromList [("Authorization", "Bearer " ++ registryConfig ^. token)]
  , _httpRequestRequestBody = ""
  }
