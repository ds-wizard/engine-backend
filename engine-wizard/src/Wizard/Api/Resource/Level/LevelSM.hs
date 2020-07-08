module Wizard.Api.Resource.Level.LevelSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Database.Migration.Development.Level.Data.Levels
import Wizard.Model.Level.Level

instance ToSchema Level where
  declareNamedSchema = simpleToSchema' "_level" level1
