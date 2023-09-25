module Wizard.Database.Migration.Development.App.Data.Apps where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.App
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Model.App.App

defaultApp :: App
defaultApp =
  App
    { uuid = defaultAppUuid
    , appId = "default"
    , name = "Default App"
    , serverDomain = "localhost:3000"
    , serverUrl = "http://localhost:3000/wizard-api"
    , clientUrl = "http://localhost:8080/wizard"
    , adminServerUrl = Nothing
    , adminClientUrl = Nothing
    , enabled = True
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentApp :: App
differentApp =
  App
    { uuid = differentAppUuid
    , appId = "different"
    , name = "Different App"
    , serverDomain = "different-server.example.com"
    , serverUrl = "https://different-server.example.com/wizard-api"
    , clientUrl = "https://different-client.example.com/wizard"
    , adminServerUrl = Nothing
    , adminClientUrl = Nothing
    , enabled = True
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentAppEdited :: App
differentAppEdited =
  differentApp
    { appId = "different-edited"
    , name = "EDtIED:Different App"
    , serverDomain = "different-edited."
    , serverUrl = "https://different-edited./wizard-api"
    , clientUrl = "https://different-edited./wizard"
    }

appCreateDto :: AppCreateDTO
appCreateDto =
  AppCreateDTO
    { appId = "new-app-id"
    , appName = "New App"
    , firstName = "Max"
    , lastName = "Planck"
    , email = "max.planck@example.com"
    , password = "password"
    }
