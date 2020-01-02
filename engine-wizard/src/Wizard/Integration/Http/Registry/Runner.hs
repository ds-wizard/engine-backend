module Wizard.Integration.Http.Registry.Runner
  ( retrievePackages
  , retrievePackageBundleById
    -- Helpers
  , heRetrievePackages
  ) where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BSL

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Registry.RequestMapper
import Wizard.Integration.Http.Registry.ResponseMapper
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics

retrievePackages :: AppConfigRegistry -> InstanceStatistics -> AppContextM (Either AppError [PackageSimpleIDTO])
retrievePackages registryConfig iStat =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackagesRequest registryConfig iStat) toRetrievePackagesResponse
    else return . Right $ []

retrievePackageBundleById :: AppConfigRegistry -> String -> AppContextM (Either AppError BSL.ByteString)
retrievePackageBundleById registryConfig pkgId =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackageBundleByIdRequest registryConfig pkgId) toRetrievePackageBundleByIdResponse
    else return . Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

-- --------------------------------
-- HELPERS
-- --------------------------------
heRetrievePackages registryConfig iStat callback = do
  eitherResult <- retrievePackages registryConfig iStat
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
