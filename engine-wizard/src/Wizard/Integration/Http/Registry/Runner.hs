module Wizard.Integration.Http.Registry.Runner where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Servant

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.Error.Error
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Integration.Http.Registry.RequestMapper
import Wizard.Integration.Http.Registry.ResponseMapper
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Service.Config.AppConfigService

retrieveOrganizations :: AppContextM [OrganizationSimpleDTO]
retrieveOrganizations = do
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  if appConfig ^. knowledgeModelRegistry . enabled
    then runRequest
           (toRetrieveOrganizationsRequest (serverConfig ^. registry) (appConfig ^. knowledgeModelRegistry))
           toRetrieveOrganizationsResponse
    else return []

createOrganization :: OrganizationCreateDTO -> AppContextM OrganizationDTO
createOrganization reqDto = do
  serverConfig <- asks _appContextServerConfig
  let request = toCreateOrganizationRequest serverConfig reqDto
  res <- runRegistryClient request
  return . getResponse $ res

confirmOrganizationRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmOrganizationRegistration reqDto = do
  serverConfig <- asks _appContextServerConfig
  let request = toConfirmOrganizationRegistrationRequest serverConfig reqDto
  res <- runRegistryClient request
  return . getResponse $ res

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
