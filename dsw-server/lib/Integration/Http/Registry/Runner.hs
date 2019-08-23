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
import Model.Statistics.InstanceStatistics

retrievePackages :: AppConfigRegistry -> InstanceStatistics -> AppContextM (Either AppError [PackageSimpleIDTO])
retrievePackages registryConfig iStat =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackagesRequest registryConfig iStat) toRetrievePackagesResponse
    else return . Right $ []

retrievePackageBundleById :: AppConfigRegistry -> String -> AppContextM (Either AppError BSL.ByteString)
retrievePackageBundleById registryConfig pkgId =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackageBundleByIdRequest registryConfig pkgId) toRetrievePackageBundleByIdResponse
    else return . Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

-- --------------------------------
-- HELPERS
-- --------------------------------
heRetrievePackages registryConfig iStat callback = do
  eitherResult <- retrievePackages registryConfig iStat
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
