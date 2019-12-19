module Wizard.Api.Handler.Level.LevelHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Service.Level.LevelService

getLevelsA :: Endpoint
getLevelsA =
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDto <- runInAuthService getLevels
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error
