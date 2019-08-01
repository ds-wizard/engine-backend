module Api.Handler.Level.LevelHandler where

import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Level.LevelJM ()
import Service.Level.LevelService

getLevelsA :: Endpoint
getLevelsA =
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDto <- runInAuthService getLevels
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error
