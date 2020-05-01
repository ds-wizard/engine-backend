module Wizard.Api.Resource.Token.TokenCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenCreateJM ()
import Wizard.Database.Migration.Development.Token.Data.Tokens

instance ToSchema TokenCreateDTO where
  declareNamedSchema = simpleToSchema albertCreateToken
