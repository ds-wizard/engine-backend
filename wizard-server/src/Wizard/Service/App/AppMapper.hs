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
    , serverDomain = createServerDomain serverConfig reqDto.appId
    , serverUrl = createServerUrl serverConfig reqDto.appId
    , clientUrl = createClientUrl serverConfig reqDto.appId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig reqDto.appId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig reqDto.appId
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
    , serverDomain = createServerDomain serverConfig reqDto.appId
    , serverUrl = createServerUrl serverConfig reqDto.appId
    , clientUrl = createClientUrl serverConfig reqDto.appId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig reqDto.appId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig reqDto.appId
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
    , serverDomain = createServerDomain serverConfig command.appId
    , serverUrl = createServerUrl serverConfig command.appId
    , clientUrl = createClientUrl serverConfig command.appId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig command.appId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig command.appId
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
    , serverDomain = createServerDomain serverConfig reqDto.appId
    , serverUrl = createServerUrl serverConfig reqDto.appId
    , clientUrl = createClientUrl serverConfig reqDto.appId
    , adminServerUrl = Just $ createAdminServerUrl serverConfig reqDto.appId
    , adminClientUrl = Just $ createAdminClientUrl serverConfig reqDto.appId
    , enabled = app.enabled
    , createdAt = app.createdAt
    , updatedAt = app.updatedAt
    }

createServerDomain :: ServerConfig -> String -> String
createServerDomain serverConfig appId = f' "%s.%s" [appId, fromMaybe "" serverConfig.cloud.domain]

createServerUrl :: ServerConfig -> String -> String
createServerUrl serverConfig appId = f' "%s/wizard-api" [createUrl serverConfig appId]

createClientUrl :: ServerConfig -> String -> String
createClientUrl serverConfig appId = f' "%s/wizard" [createUrl serverConfig appId]

createAdminServerUrl :: ServerConfig -> String -> String
createAdminServerUrl serverConfig appId = f' "%s/admin-api" [createUrl serverConfig appId]

createAdminClientUrl :: ServerConfig -> String -> String
createAdminClientUrl serverConfig appId = f' "%s/admin" [createUrl serverConfig appId]

createUrl :: ServerConfig -> String -> String
createUrl serverConfig appId = f' "https://%s.%s" [appId, fromMaybe "" serverConfig.cloud.domain]
