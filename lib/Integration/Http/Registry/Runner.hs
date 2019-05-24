module Integration.Http.Registry.Runner
  ( retrievePackages
  , retrievePackageBundleById
    -- Helpers
  , heRetrievePackages
  ) where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BSL

import Integration.Http.Common.HttpClient
import Integration.Http.Registry.RequestMapper
import Integration.Http.Registry.ResponseMapper
import Integration.Resource.Package.PackageSimpleIDTO
import LensesConfig
import Localization
import Model.Config.AppConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers

retrievePackages :: AppConfigRegistry -> AppContextM (Either AppError [PackageSimpleIDTO])
retrievePackages registryConfig =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackagesRequest registryConfig) toRetrievePackagesResponse
    else return . Right $ []

retrievePackageBundleById :: AppConfigRegistry -> String -> AppContextM (Either AppError BSL.ByteString)
retrievePackageBundleById registryConfig pkgId =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackageBundleByIdRequest registryConfig pkgId) toRetrievePackageBundleByIdResponse
    else return . Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

-- --------------------------------
-- HELPERS
-- --------------------------------
heRetrievePackages registryConfig callback = do
  eitherResult <- retrievePackages registryConfig
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
