module Wizard.Service.App.AppMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Model.App.App

fromCreate :: AppCreateDTO -> U.UUID -> UTCTime -> App
fromCreate reqDto aUuid now =
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
