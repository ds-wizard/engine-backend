module Wizard.Service.App.AppMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.String
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Plan.AppPlan
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper
import WizardLib.Public.Model.PersistentCommand.App.CreateOrUpdateAppCommand

toDTO :: App -> Maybe String -> Maybe String -> AppDTO
toDTO app mLogoUrl mPrimaryColor =
  AppDTO
    { uuid = app.uuid
    , appId = app.appId
    , name = app.name
    , serverDomain = app.serverDomain
    , serverUrl = app.serverUrl
    , clientUrl = app.clientUrl
    , enabled = app.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , createdAt = app.createdAt
    , updatedAt = app.updatedAt
    }

toDetailDTO :: App -> Maybe String -> Maybe String -> [AppPlan] -> UsageDTO -> [User] -> AppDetailDTO
toDetailDTO app mLogoUrl mPrimaryColor plans usage users =
  AppDetailDTO
    { uuid = app.uuid
    , appId = app.appId
    , name = app.name
    , serverDomain = app.serverDomain
    , serverUrl = app.serverUrl
    , clientUrl = app.clientUrl
    , enabled = app.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , plans = plans
    , usage = usage
    , users = fmap U_Mapper.toDTO users
    , createdAt = app.createdAt
    , updatedAt = app.updatedAt
    }

toChangeDTO :: App -> AppChangeDTO
toChangeDTO app = AppChangeDTO {appId = app.appId, name = app.name}

fromRegisterCreateDTO :: AppCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> App
fromRegisterCreateDTO reqDto aUuid serverConfig now =
  App
    { uuid = aUuid
    , appId = reqDto.appId
    , name = reqDto.appId
    , serverDomain = f' "api-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , serverUrl = f' "https://api-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , clientUrl = f' "https://%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminServerUrl = Just $ f' "https://api-admin-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminClientUrl = Just $ f' "https://admin-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromAdminCreateDTO :: AppCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> App
fromAdminCreateDTO reqDto aUuid serverConfig now =
  App
    { uuid = aUuid
    , appId = reqDto.appId
    , name = reqDto.appName
    , serverDomain = f' "api-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , serverUrl = f' "https://api-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , clientUrl = f' "https://%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminServerUrl = Just $ f' "https://api-admin-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminClientUrl = Just $ f' "https://admin-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromCommand :: CreateOrUpdateAppCommand -> ServerConfig -> UTCTime -> App
fromCommand command serverConfig now =
  App
    { uuid = command.uuid
    , appId = command.appId
    , name = command.name
    , serverDomain = f' "api-%s%s.%s" [serverConfig.cloud.prefix, command.appId, fromMaybe "" serverConfig.cloud.domain]
    , serverUrl = f' "https://api-%s%s.%s" [serverConfig.cloud.prefix, command.appId, fromMaybe "" serverConfig.cloud.domain]
    , clientUrl = f' "https://%s%s.%s" [serverConfig.cloud.prefix, command.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminServerUrl = Just $ f' "https://api-admin-%s%s.%s" [serverConfig.cloud.prefix, command.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminClientUrl = Just $ f' "https://admin-%s%s.%s" [serverConfig.cloud.prefix, command.appId, fromMaybe "" serverConfig.cloud.domain]
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: App -> AppChangeDTO -> ServerConfig -> App
fromChangeDTO app reqDto serverConfig =
  App
    { uuid = app.uuid
    , appId = reqDto.appId
    , name = reqDto.name
    , serverDomain = f' "api-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , serverUrl = f' "https://api-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , clientUrl = f' "https://%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminServerUrl = Just $ f' "https://api-admin-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , adminClientUrl = Just $ f' "https://admin-%s%s.%s" [serverConfig.cloud.prefix, reqDto.appId, fromMaybe "" serverConfig.cloud.domain]
    , enabled = app.enabled
    , createdAt = app.createdAt
    , updatedAt = app.updatedAt
    }
