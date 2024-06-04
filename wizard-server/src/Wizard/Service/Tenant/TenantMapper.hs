module Wizard.Service.Tenant.TenantMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.String
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper
import WizardLib.Public.Model.PersistentCommand.Tenant.CreateOrUpdateTenantCommand
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

toDTO :: Tenant -> Maybe String -> Maybe String -> TenantDTO
toDTO tenant mLogoUrl mPrimaryColor =
  TenantDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenant.serverUrl
    , clientUrl = tenant.clientUrl
    , enabled = tenant.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

toDetailDTO :: Tenant -> Maybe String -> Maybe String -> [TenantPlan] -> TenantUsageDTO -> [User] -> TenantDetailDTO
toDetailDTO tenant mLogoUrl mPrimaryColor plans usage users =
  TenantDetailDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenant.serverUrl
    , clientUrl = tenant.clientUrl
    , enabled = tenant.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , plans = plans
    , usage = usage
    , users = fmap U_Mapper.toDTO users
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

toChangeDTO :: Tenant -> TenantChangeDTO
toChangeDTO tenant = TenantChangeDTO {tenantId = tenant.tenantId, name = tenant.name}

fromRegisterCreateDTO :: TenantCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> Tenant
fromRegisterCreateDTO reqDto aUuid serverConfig now =
  Tenant
    { uuid = aUuid
    , tenantId = reqDto.tenantId
    , name = reqDto.tenantId
    , serverDomain = createServerDomain serverConfig reqDto.tenantId
    , serverUrl = createServerUrl serverConfig reqDto.tenantId
    , clientUrl = createClientUrl serverConfig reqDto.tenantId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig reqDto.tenantId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig reqDto.tenantId
    , integrationHubServerUrl = Just $ createIntegrationHubServerUrl serverConfig reqDto.tenantId
    , integrationHubClientUrl = Just $ createIntegrationHubClientUrl serverConfig reqDto.tenantId
    , analyticsServerUrl = Just $ createReportingServerUrl serverConfig reqDto.tenantId
    , analyticsClientUrl = Just $ createReportingClientUrl serverConfig reqDto.tenantId
    , signalBridgeUrl = serverConfig.cloud.signalBridgeUrl
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromAdminCreateDTO :: TenantCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> Tenant
fromAdminCreateDTO reqDto aUuid serverConfig now =
  Tenant
    { uuid = aUuid
    , tenantId = reqDto.tenantId
    , name = reqDto.tenantName
    , serverDomain = createServerDomain serverConfig reqDto.tenantId
    , serverUrl = createServerUrl serverConfig reqDto.tenantId
    , clientUrl = createClientUrl serverConfig reqDto.tenantId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig reqDto.tenantId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig reqDto.tenantId
    , integrationHubServerUrl = Just $ createIntegrationHubServerUrl serverConfig reqDto.tenantId
    , integrationHubClientUrl = Just $ createIntegrationHubClientUrl serverConfig reqDto.tenantId
    , analyticsServerUrl = Just $ createReportingServerUrl serverConfig reqDto.tenantId
    , analyticsClientUrl = Just $ createReportingClientUrl serverConfig reqDto.tenantId
    , signalBridgeUrl = serverConfig.cloud.signalBridgeUrl
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromCommand :: CreateOrUpdateTenantCommand -> ServerConfig -> UTCTime -> Tenant
fromCommand command serverConfig now =
  Tenant
    { uuid = command.uuid
    , tenantId = command.tenantId
    , name = command.name
    , serverDomain = createServerDomain serverConfig command.tenantId
    , serverUrl = createServerUrl serverConfig command.tenantId
    , clientUrl = createClientUrl serverConfig command.tenantId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig command.tenantId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig command.tenantId
    , integrationHubServerUrl = Just $ createIntegrationHubServerUrl serverConfig command.tenantId
    , integrationHubClientUrl = Just $ createIntegrationHubClientUrl serverConfig command.tenantId
    , analyticsServerUrl = Just $ createReportingServerUrl serverConfig command.tenantId
    , analyticsClientUrl = Just $ createReportingClientUrl serverConfig command.tenantId
    , signalBridgeUrl = serverConfig.cloud.signalBridgeUrl
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: Tenant -> TenantChangeDTO -> ServerConfig -> Tenant
fromChangeDTO tenant reqDto serverConfig =
  Tenant
    { uuid = tenant.uuid
    , tenantId = reqDto.tenantId
    , name = reqDto.name
    , serverDomain = createServerDomain serverConfig reqDto.tenantId
    , serverUrl = createServerUrl serverConfig reqDto.tenantId
    , clientUrl = createClientUrl serverConfig reqDto.tenantId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig reqDto.tenantId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig reqDto.tenantId
    , integrationHubServerUrl = Just $ createIntegrationHubServerUrl serverConfig reqDto.tenantId
    , integrationHubClientUrl = Just $ createIntegrationHubClientUrl serverConfig reqDto.tenantId
    , analyticsServerUrl = Just $ createReportingServerUrl serverConfig reqDto.tenantId
    , analyticsClientUrl = Just $ createReportingClientUrl serverConfig reqDto.tenantId
    , signalBridgeUrl = tenant.signalBridgeUrl
    , enabled = tenant.enabled
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

createServerDomain :: ServerConfig -> String -> String
createServerDomain serverConfig tenantId = f' "%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]

createServerUrl :: ServerConfig -> String -> String
createServerUrl serverConfig tenantId = f' "%s/wizard-api" [createUrl serverConfig tenantId]

createClientUrl :: ServerConfig -> String -> String
createClientUrl serverConfig tenantId = f' "%s/wizard" [createUrl serverConfig tenantId]

createAdminServerUrl :: ServerConfig -> String -> String
createAdminServerUrl serverConfig tenantId = f' "%s/admin-api" [createUrl serverConfig tenantId]

createAdminClientUrl :: ServerConfig -> String -> String
createAdminClientUrl serverConfig tenantId = f' "%s/admin" [createUrl serverConfig tenantId]

createIntegrationHubServerUrl :: ServerConfig -> String -> String
createIntegrationHubServerUrl serverConfig tenantId = f' "%s/integration-hub-api" [createUrl serverConfig tenantId]

createIntegrationHubClientUrl :: ServerConfig -> String -> String
createIntegrationHubClientUrl serverConfig tenantId = f' "%s/integration-hub" [createUrl serverConfig tenantId]

createReportingServerUrl :: ServerConfig -> String -> String
createReportingServerUrl serverConfig tenantId = f' "%s/analytics-api" [createUrl serverConfig tenantId]

createReportingClientUrl :: ServerConfig -> String -> String
createReportingClientUrl serverConfig tenantId = f' "%s/analytics" [createUrl serverConfig tenantId]

createUrl :: ServerConfig -> String -> String
createUrl serverConfig tenantId = f' "https://%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]
