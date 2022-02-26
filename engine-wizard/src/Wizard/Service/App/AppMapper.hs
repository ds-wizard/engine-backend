module Wizard.Service.App.AppMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.String
import Wizard.Api.Resource.App.AppAdminCreateDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Model.App.App

toDTO :: App -> AppDTO
toDTO app = AppDTO {_appDTOClientUrl = app ^. clientUrl}

fromCreateDTO :: AppCreateDTO -> U.UUID -> String -> UTCTime -> App
fromCreateDTO reqDto aUuid cloudDomain now =
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

fromAdminCreate :: AppAdminCreateDTO -> U.UUID -> UTCTime -> App
fromAdminCreate reqDto aUuid now =
  App
    { _appUuid = aUuid
    , _appAppId = reqDto ^. appId
    , _appName = reqDto ^. name
    , _appServerDomain = reqDto ^. serverDomain
    , _appServerUrl = reqDto ^. serverUrl
    , _appClientUrl = reqDto ^. clientUrl
    , _appEnabled = True
    , _appCreatedAt = now
    , _appUpdatedAt = now
    }
