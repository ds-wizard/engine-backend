module Api.Handler.Level.LevelHandler where

import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Level.LevelJS ()
import Service.Level.LevelService

getLevelsA :: Endpoint
getLevelsA = do
  eitherDto <- lift $ getLevels
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error
