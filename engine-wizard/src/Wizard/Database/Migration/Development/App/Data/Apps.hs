module Wizard.Database.Migration.Development.App.Data.Apps where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.App
import Shared.Util.Uuid
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Model.App.App

defaultApp :: App
defaultApp =
  App
    { _appUuid = defaultAppUuid
    , _appAppId = "default"
    , _appName = "Default App"
    , _appServerDomain = "localhost:3000"
    , _appServerUrl = "http://localhost:3000"
    , _appClientUrl = "http://localhost:8080"
    , _appEnabled = True
    , _appCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _appUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentApp :: App
differentApp =
  App
    { _appUuid = u' "d9e73946-faa6-449d-83e4-2e38371b7bfa"
    , _appAppId = "different"
    , _appName = "Different App"
    , _appServerDomain = "different-server.example.com"
    , _appServerUrl = "https://different-server.example.com"
    , _appClientUrl = "https://different-client.example.com"
    , _appEnabled = True
    , _appCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _appUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

appCreateDto :: AppCreateDTO
appCreateDto =
  AppCreateDTO
    { _appCreateDTOAppId = "new-app-id"
    , _appCreateDTOFirstName = "Max"
    , _appCreateDTOLastName = "Planck"
    , _appCreateDTOEmail = "max.planck@example.com"
    , _appCreateDTOPassword = "password"
    }
