module Wizard.Api.Resource.Level.LevelSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Level.LevelDTO
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Database.Migration.Development.Level.Data.Levels
import Wizard.Service.Level.LevelMapper

instance ToSchema LevelDTO where
  declareNamedSchema = simpleToSchema (toLevelDTO level1)
