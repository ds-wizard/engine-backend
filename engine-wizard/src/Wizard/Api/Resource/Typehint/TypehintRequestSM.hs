module Wizard.Api.Resource.Typehint.TypehintRequestSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Api.Resource.Typehint.TypehintRequestJM ()
import Wizard.Database.Migration.Development.Typehint.Data.Typehints

instance ToSchema TypehintRequestDTO where
  declareNamedSchema = simpleToSchema typehintRequest
