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
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Model.PersistentCommand.Tenant.CreateOrUpdateTenantCommand

toDTO :: Tenant -> Maybe String -> Maybe String -> TenantDTO
toDTO tenant mLogoUrl mPrimaryColor =
  TenantDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenant.serverUrl
    , clientUrl = tenant.clientUrl
    , state = tenant.state
    , enabled = tenant.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

toDetailDTO :: Tenant -> Maybe String -> Maybe String -> WizardUsageDTO -> [User] -> TenantDetailDTO
toDetailDTO tenant mLogoUrl mPrimaryColor usage users =
  TenantDetailDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenant.serverUrl
    , clientUrl = tenant.clientUrl
    , state = tenant.state
    , enabled = tenant.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , usage = usage
    , users = fmap U_Mapper.toDTO users
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

toChangeDTO :: Tenant -> TenantChangeDTO
toChangeDTO tenant = TenantChangeDTO {tenantId = tenant.tenantId, name = tenant.name}

fromRegisterCreateDTO :: TenantCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> Tenant
fromRegisterCreateDTO reqDto aUuid serverConfig now =
  let url = createUrl serverConfig reqDto.tenantId
   in Tenant
        { uuid = aUuid
        , tenantId = reqDto.tenantId
        , name = reqDto.tenantId
        , serverDomain = createServerDomain serverConfig reqDto.tenantId
        , serverUrl = createServerUrl url
        , clientUrl = createClientUrl url
        , adminServerUrl = Just $ createAdminServerUrl url
        , adminClientUrl = Just $ createAdminClientUrl url
        , integrationHubServerUrl = Just $ createIntegrationHubServerUrl url
        , integrationHubClientUrl = Just $ createIntegrationHubClientUrl url
        , analyticsServerUrl = Just $ createReportingServerUrl url
        , analyticsClientUrl = Just $ createReportingClientUrl url
        , signalBridgeUrl = serverConfig.cloud.signalBridgeUrl
        , enabled = True
        , state = ReadyForUseTenantState
        , createdAt = now
        , updatedAt = now
        }

fromAdminCreateDTO :: TenantCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> Tenant
fromAdminCreateDTO reqDto aUuid serverConfig now =
  let url = createUrl serverConfig reqDto.tenantId
   in Tenant
        { uuid = aUuid
        , tenantId = reqDto.tenantId
        , name = reqDto.tenantName
        , serverDomain = createServerDomain serverConfig reqDto.tenantId
        , serverUrl = createServerUrl url
        , clientUrl = createClientUrl url
        , adminServerUrl = Just $ createAdminServerUrl url
        , adminClientUrl = Just $ createAdminClientUrl url
        , integrationHubServerUrl = Just $ createIntegrationHubServerUrl url
        , integrationHubClientUrl = Just $ createIntegrationHubClientUrl url
        , analyticsServerUrl = Just $ createReportingServerUrl url
        , analyticsClientUrl = Just $ createReportingClientUrl url
        , signalBridgeUrl = serverConfig.cloud.signalBridgeUrl
        , enabled = True
        , state = ReadyForUseTenantState
        , createdAt = now
        , updatedAt = now
        }

fromCommand :: CreateOrUpdateTenantCommand -> TenantState -> ServerConfig -> UTCTime -> UTCTime -> Tenant
fromCommand command state serverConfig createdAt updatedAt =
  let (serverDomain, url) =
        case command.customDomain of
          Just customDomain -> (customDomain, f' "https://%s" [customDomain])
          Nothing -> (createServerDomain serverConfig command.tenantId, createUrl serverConfig command.tenantId)
   in Tenant
        { uuid = command.uuid
        , tenantId = command.tenantId
        , name = command.name
        , serverDomain = serverDomain
        , serverUrl = createServerUrl url
        , clientUrl = createClientUrl url
        , adminServerUrl = Just $ createAdminServerUrl url
        , adminClientUrl = Just $ createAdminClientUrl url
        , integrationHubServerUrl = Just $ createIntegrationHubServerUrl url
        , integrationHubClientUrl = Just $ createIntegrationHubClientUrl url
        , analyticsServerUrl = Just $ createReportingServerUrl url
        , analyticsClientUrl = Just $ createReportingClientUrl url
        , signalBridgeUrl = serverConfig.cloud.signalBridgeUrl
        , enabled = command.enabled
        , state = state
        , createdAt = createdAt
        , updatedAt = updatedAt
        }

fromChangeDTO :: Tenant -> TenantChangeDTO -> ServerConfig -> Tenant
fromChangeDTO tenant reqDto serverConfig =
  let url = createUrl serverConfig reqDto.tenantId
   in Tenant
        { uuid = tenant.uuid
        , tenantId = reqDto.tenantId
        , name = reqDto.name
        , serverDomain = createServerDomain serverConfig reqDto.tenantId
        , serverUrl = createServerUrl url
        , clientUrl = createClientUrl url
        , adminServerUrl = Just $ createAdminServerUrl url
        , adminClientUrl = Just $ createAdminClientUrl url
        , integrationHubServerUrl = Just $ createIntegrationHubServerUrl url
        , integrationHubClientUrl = Just $ createIntegrationHubClientUrl url
        , analyticsServerUrl = Just $ createReportingServerUrl url
        , analyticsClientUrl = Just $ createReportingClientUrl url
        , signalBridgeUrl = tenant.signalBridgeUrl
        , enabled = tenant.enabled
        , state = tenant.state
        , createdAt = tenant.createdAt
        , updatedAt = tenant.updatedAt
        }

createServerDomain :: ServerConfig -> String -> String
createServerDomain serverConfig tenantId = f' "%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]

createServerUrl :: String -> String
createServerUrl url = f' "%s/wizard-api" [url]

createClientUrl :: String -> String
createClientUrl url = f' "%s/wizard" [url]

createAdminServerUrl :: String -> String
createAdminServerUrl url = f' "%s/admin-api" [url]

createAdminClientUrl :: String -> String
createAdminClientUrl url = f' "%s/admin" [url]

createIntegrationHubServerUrl :: String -> String
createIntegrationHubServerUrl url = f' "%s/integration-hub-api" [url]

createIntegrationHubClientUrl :: String -> String
createIntegrationHubClientUrl url = f' "%s/integration-hub" [url]

createReportingServerUrl :: String -> String
createReportingServerUrl url = f' "%s/analytics-api" [url]

createReportingClientUrl :: String -> String
createReportingClientUrl url = f' "%s/analytics" [url]

createUrl :: ServerConfig -> String -> String
createUrl serverConfig tenantId = f' "https://%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]
