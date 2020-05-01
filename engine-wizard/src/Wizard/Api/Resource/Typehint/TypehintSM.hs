module Wizard.Api.Resource.Typehint.TypehintSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintJM ()
import Wizard.Database.Migration.Development.Typehint.Data.Typehints

instance ToSchema TypehintDTO where
  declareNamedSchema = simpleToSchema lifeScienceTypehint
