module Wizard.Service.App.AppMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.String
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Model.App.App
import Wizard.Model.Plan.AppPlan
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toDTO :: App -> Maybe String -> Maybe String -> AppDTO
toDTO app mLogoUrl mPrimaryColor =
  AppDTO
    { _appDTOUuid = app ^. uuid
    , _appDTOAppId = app ^. appId
    , _appDTOName = app ^. name
    , _appDTOServerDomain = app ^. serverDomain
    , _appDTOServerUrl = app ^. serverUrl
    , _appDTOClientUrl = app ^. clientUrl
    , _appDTOEnabled = app ^. enabled
    , _appDTOLogoUrl = mLogoUrl
    , _appDTOPrimaryColor = mPrimaryColor
    , _appDTOCreatedAt = app ^. createdAt
    , _appDTOUpdatedAt = app ^. updatedAt
    }

toDetailDTO :: App -> [AppPlan] -> UsageDTO -> [User] -> AppDetailDTO
toDetailDTO app plans usage users =
  AppDetailDTO
    { _appDetailDTOUuid = app ^. uuid
    , _appDetailDTOAppId = app ^. appId
    , _appDetailDTOName = app ^. name
    , _appDetailDTOServerDomain = app ^. serverDomain
    , _appDetailDTOServerUrl = app ^. serverUrl
    , _appDetailDTOClientUrl = app ^. clientUrl
    , _appDetailDTOEnabled = app ^. enabled
    , _appDetailDTOPlans = plans
    , _appDetailDTOUsage = usage
    , _appDetailDTOUsers = fmap U_Mapper.toDTO users
    , _appDetailDTOCreatedAt = app ^. createdAt
    , _appDetailDTOUpdatedAt = app ^. updatedAt
    }

toChangeDTO :: App -> AppChangeDTO
toChangeDTO app = AppChangeDTO {_appChangeDTOAppId = app ^. appId, _appChangeDTOName = app ^. name}

fromRegisterCreateDTO :: AppCreateDTO -> U.UUID -> String -> UTCTime -> App
fromRegisterCreateDTO reqDto aUuid cloudDomain now =
  App
    { _appUuid = aUuid
    , _appAppId = reqDto ^. appId
    , _appName = reqDto ^. appId
    , _appServerDomain = f' "api-%s.%s" [reqDto ^. appId, cloudDomain]
    , _appServerUrl = f' "https://api-%s.%s" [reqDto ^. appId, cloudDomain]
    , _appClientUrl = f' "https://%s.%s" [reqDto ^. appId, cloudDomain]
    , _appEnabled = True
    , _appCreatedAt = now
    , _appUpdatedAt = now
    }

fromAdminCreateDTO :: AppCreateDTO -> U.UUID -> String -> UTCTime -> App
fromAdminCreateDTO reqDto aUuid cloudDomain now =
  App
    { _appUuid = aUuid
    , _appAppId = reqDto ^. appId
    , _appName = reqDto ^. appName
    , _appServerDomain = f' "api-%s.%s" [reqDto ^. appId, cloudDomain]
    , _appServerUrl = f' "https://api-%s.%s" [reqDto ^. appId, cloudDomain]
    , _appClientUrl = f' "https://%s.%s" [reqDto ^. appId, cloudDomain]
    , _appEnabled = True
    , _appCreatedAt = now
    , _appUpdatedAt = now
    }

fromChangeDTO :: App -> AppChangeDTO -> String -> App
fromChangeDTO app reqDto cloudDomain =
  App
    { _appUuid = app ^. uuid
    , _appAppId = reqDto ^. appId
    , _appName = reqDto ^. name
    , _appServerDomain = f' "api-%s.%s" [reqDto ^. appId, cloudDomain]
    , _appServerUrl = f' "https://api-%s.%s" [reqDto ^. appId, cloudDomain]
    , _appClientUrl = f' "https://%s.%s" [reqDto ^. appId, cloudDomain]
    , _appEnabled = app ^. enabled
    , _appCreatedAt = app ^. createdAt
    , _appUpdatedAt = app ^. updatedAt
    }
