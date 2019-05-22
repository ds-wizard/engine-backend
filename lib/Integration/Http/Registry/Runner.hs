module Integration.Http.Registry.Runner
  ( retrievePackages
  , retrievePackageBundleById
    -- Helpers
  , heRetrievePackages
  ) where

import qualified Data.ByteString.Lazy as BSL

import Integration.Http.Common.HttpClient
import Integration.Http.Registry.RequestMapper
import Integration.Http.Registry.ResponseMapper
import Integration.Resource.Package.PackageSimpleIDTO
import Model.Config.AppConfig
import Model.Context.AppContext
import Model.Error.Error

retrievePackages :: AppConfigRegistry -> AppContextM (Either AppError [PackageSimpleIDTO])
retrievePackages registryConfig = runRequest (toRetrievePackagesRequest registryConfig) toRetrievePackagesResponse

retrievePackageBundleById :: AppConfigRegistry -> String -> AppContextM (Either AppError BSL.ByteString)
retrievePackageBundleById registryConfig pkgId =
  runRequest (toRetrievePackageBundleByIdRequest registryConfig pkgId) toRetrievePackageBundleByIdResponse

-- --------------------------------
-- HELPERS
-- --------------------------------
heRetrievePackages registryConfig callback = do
  eitherResult <- retrievePackages registryConfig
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
