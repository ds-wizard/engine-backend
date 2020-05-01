module Wizard.Api.Resource.Token.TokenSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Api.Resource.Token.TokenJM ()
import Wizard.Database.Migration.Development.Token.Data.Tokens

instance ToSchema TokenDTO where
  declareNamedSchema = simpleToSchema albertToken
