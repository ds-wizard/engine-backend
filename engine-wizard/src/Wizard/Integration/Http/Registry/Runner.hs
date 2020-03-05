module Wizard.Integration.Http.Registry.Runner
  ( retrievePackages
  , retrievePackageBundleById
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
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

retrievePackages :: AppConfigRegistry -> InstanceStatistics -> AppContextM [PackageSimpleIDTO]
retrievePackages registryConfig iStat =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackagesRequest registryConfig iStat) toRetrievePackagesResponse
    else return []

retrievePackageBundleById :: AppConfigRegistry -> String -> AppContextM BSL.ByteString
retrievePackageBundleById registryConfig pkgId =
  if registryConfig ^. enabled
    then runRequest (toRetrievePackageBundleByIdRequest registryConfig pkgId) toRetrievePackageBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"
