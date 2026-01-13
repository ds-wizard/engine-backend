module Wizard.Integration.Http.Registry.Runner where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Servant

import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationStateJM ()
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Integration.Http.Common.HttpClient
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Error.Error
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Integration.Http.Registry.RequestMapper
import Wizard.Integration.Http.Registry.ResponseMapper
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper

retrieveOrganizations :: AppContextM [OrganizationSimple]
retrieveOrganizations = do
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then do
      let request = toRetrieveOrganizationsRequest
      res <- runRegistryClient request
      return . getResponse $ res
    else return []

createOrganization :: OrganizationCreateDTO -> AppContextM OrganizationDTO
createOrganization reqDto = do
  serverConfig <- asks serverConfig
  clientUrl <- getClientUrl
  let request = toCreateOrganizationRequest serverConfig reqDto clientUrl
  res <- runRegistryClient request
  return . getResponse $ res

confirmOrganizationRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmOrganizationRegistration reqDto = do
  let request = toConfirmOrganizationRegistrationRequest reqDto
  res <- runRegistryClient request
  return . getResponse $ res

retrievePackages :: InstanceStatistics -> AppContextM [KnowledgeModelPackageSimpleDTO]
retrievePackages iStat = do
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      catchError
        ( do
            let request = toRetrievePackagesRequest tcRegistry iStat
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveKnowledgeModelBundleById :: String -> AppContextM BSL.ByteString
retrieveKnowledgeModelBundleById pkgId = do
  serverConfig <- asks serverConfig
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      runRequest
        (toRetrieveKnowledgeModelBundleByIdRequest serverConfig.registry tcRegistry pkgId)
        toRetrieveKnowledgeModelBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveTemplates :: AppContextM [DocumentTemplateSimpleDTO]
retrieveTemplates = do
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      catchError
        ( do
            let request = toRetrieveTemplatesRequest tcRegistry
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveTemplateBundleById :: String -> AppContextM BSL.ByteString
retrieveTemplateBundleById tmlId = do
  serverConfig <- asks serverConfig
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      runRequest
        (toRetrieveTemplateBundleByIdRequest serverConfig.registry tcRegistry tmlId)
        toRetrieveTemplateBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

retrieveLocales :: AppContextM [LocaleDTO]
retrieveLocales = do
  buildInfoConfig <- asks buildInfoConfig
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      catchError
        ( do
            let request = toRetrieveLocaleRequest buildInfoConfig.releaseVersion tcRegistry
            res <- runRegistryClient request
            return . getResponse $ res
        )
        (\_ -> return [])
    else return []

retrieveLocaleBundleByCoordinate :: Coordinate -> AppContextM BSL.ByteString
retrieveLocaleBundleByCoordinate coordinate = do
  serverConfig <- asks serverConfig
  tcRegistry <- getCurrentTenantConfigRegistry
  if tcRegistry.enabled
    then
      runRequest
        (toRetrieveLocaleBundleByIdRequest serverConfig.registry tcRegistry coordinate)
        toRetrieveLocaleBundleByIdResponse
    else throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registry"

uploadKnowledgeModelBundle :: KnowledgeModelBundle -> AppContextM KnowledgeModelBundle
uploadKnowledgeModelBundle reqDto = do
  tcRegistry <- getCurrentTenantConfigRegistry
  let request = toUploadKnowledgeModelBundleRequest tcRegistry reqDto
  res <- runRegistryClient request
  return . getResponse $ res

uploadDocumentTemplateBundle :: BSL.ByteString -> AppContextM BSL.ByteString
uploadDocumentTemplateBundle bundle = do
  serverConfig <- asks serverConfig
  tcRegistry <- getCurrentTenantConfigRegistry
  runRequest
    (toUploadDocumentTemplateBundleRequest serverConfig.registry tcRegistry bundle)
    toUploadDocumentTemplateBundleResponse

uploadLocaleBundle :: BSL.ByteString -> AppContextM BSL.ByteString
uploadLocaleBundle bundle = do
  serverConfig <- asks serverConfig
  tcRegistry <- getCurrentTenantConfigRegistry
  runRequest
    (toUploadLocaleBundleRequest serverConfig.registry tcRegistry bundle)
    toUploadLocaleBundleResponse
