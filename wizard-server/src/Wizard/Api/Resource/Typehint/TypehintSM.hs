module Wizard.Api.Resource.Typehint.TypehintSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Typehint.TypehintIJM ()
import Wizard.Database.Migration.Development.Typehint.Data.Typehints
import Wizard.Integration.Resource.Typehint.TypehintIDTO

instance ToSchema TypehintIDTO where
  declareNamedSchema = toSwagger lifeScienceTypehint
