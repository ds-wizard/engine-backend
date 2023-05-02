module Wizard.Service.App.AppMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.String
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Model.App.App
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

fromRegisterCreateDTO :: AppCreateDTO -> U.UUID -> String -> UTCTime -> App
fromRegisterCreateDTO reqDto aUuid cloudDomain now =
  App
    { uuid = aUuid
    , appId = reqDto.appId
    , name = reqDto.appId
    , serverDomain = f' "api-%s.%s" [reqDto.appId, cloudDomain]
    , serverUrl = f' "https://api-%s.%s" [reqDto.appId, cloudDomain]
    , clientUrl = f' "https://%s.%s" [reqDto.appId, cloudDomain]
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromAdminCreateDTO :: AppCreateDTO -> U.UUID -> String -> UTCTime -> App
fromAdminCreateDTO reqDto aUuid cloudDomain now =
  App
    { uuid = aUuid
    , appId = reqDto.appId
    , name = reqDto.appName
    , serverDomain = f' "api-%s.%s" [reqDto.appId, cloudDomain]
    , serverUrl = f' "https://api-%s.%s" [reqDto.appId, cloudDomain]
    , clientUrl = f' "https://%s.%s" [reqDto.appId, cloudDomain]
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromCommand :: CreateOrUpdateAppCommand -> String -> UTCTime -> App
fromCommand command cloudDomain now =
  App
    { uuid = command.uuid
    , appId = command.appId
    , name = command.name
    , serverDomain = f' "api-%s.%s" [command.appId, cloudDomain]
    , serverUrl = f' "https://api-%s.%s" [command.appId, cloudDomain]
    , clientUrl = f' "https://%s.%s" [command.appId, cloudDomain]
    , enabled = True
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: App -> AppChangeDTO -> String -> App
fromChangeDTO app reqDto cloudDomain =
  App
    { uuid = app.uuid
    , appId = reqDto.appId
    , name = reqDto.name
    , serverDomain = f' "api-%s.%s" [reqDto.appId, cloudDomain]
    , serverUrl = f' "https://api-%s.%s" [reqDto.appId, cloudDomain]
    , clientUrl = f' "https://%s.%s" [reqDto.appId, cloudDomain]
    , enabled = app.enabled
    , createdAt = app.createdAt
    , updatedAt = app.updatedAt
    }
