module Wizard.Integration.Http.Registry.Runner where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Servant

import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.Error.Error
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Integration.Http.Registry.RequestMapper
import Wizard.Integration.Http.Registry.ResponseMapper
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.AppConfigService

retrieveOrganizations :: AppContextM [OrganizationSimpleDTO]
retrieveOrganizations = do
  appConfig <- getAppConfig
  if appConfig.registry.enabled
    then do
      let request = toRetrieveOrganizationsRequest
      res <- runRegistryClient request
      return . getResponse $ res
    else return []

createOrganization :: OrganizationCreateDTO -> AppContextM OrganizationDTO
createOrganization reqDto = do
  serverConfig <- asks serverConfig
  clientUrl <- getAppClientUrl
  let request = toCreateOrganizationRequest serverConfig reqDto clientUrl
  res <- runRegistryClient request
  return . getResponse $ res

confirmOrganizationRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmOrganizationRegistration reqDto = do
  let request = toConfirmOrganizationRegistrationRequest reqDto
  res <- runRegistryClient request
  return . getResponse $ res

retrievePackages :: InstanceStatistics -> AppContextM [PackageSimpleDTO]
retrievePackages iStat = do
  appConfig <- getAppConfig
  if appConfig.registry.enabled
    then
      catchError
        ( do
            let request = toRetrievePackagesRequest appConfig.registry iStat
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrievePackageBundleById :: String -> AppContextM BSL.ByteString
retrievePackageBundleById pkgId = do
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  if appConfig.registry.enabled
    then
      runRequest
        (toRetrievePackageBundleByIdRequest serverConfig.registry appConfig.registry pkgId)
        toRetrievePackageBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveTemplates :: AppContextM [TemplateSimpleDTO]
retrieveTemplates = do
  appConfig <- getAppConfig
  if appConfig.registry.enabled
    then
      catchError
        ( do
            let request = toRetrieveTemplatesRequest appConfig.registry
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveTemplateBundleById :: String -> AppContextM BSL.ByteString
retrieveTemplateBundleById tmlId = do
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  if appConfig.registry.enabled
    then
      runRequest
        (toRetrieveTemplateBundleByIdRequest serverConfig.registry appConfig.registry tmlId)
        toRetrieveTemplateBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"
