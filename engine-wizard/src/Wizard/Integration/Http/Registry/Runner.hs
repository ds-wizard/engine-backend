module Wizard.Integration.Http.Registry.Runner
  ( retrievePackages
  , retrievePackageBundleById
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Registry.RequestMapper
import Wizard.Integration.Http.Registry.ResponseMapper
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Service.Config.AppConfigService

retrievePackages :: InstanceStatistics -> AppContextM [PackageSimpleIDTO]
retrievePackages iStat = do
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  if appConfig ^. knowledgeModelRegistry . enabled
    then runRequest
           (toRetrievePackagesRequest (serverConfig ^. registry) (appConfig ^. knowledgeModelRegistry) iStat)
           toRetrievePackagesResponse
    else return []

retrievePackageBundleById :: String -> AppContextM BSL.ByteString
retrievePackageBundleById pkgId = do
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  if appConfig ^. knowledgeModelRegistry . enabled
    then runRequest
           (toRetrievePackageBundleByIdRequest (serverConfig ^. registry) (appConfig ^. knowledgeModelRegistry) pkgId)
           toRetrievePackageBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"
