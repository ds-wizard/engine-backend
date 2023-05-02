module Wizard.Api.Resource.Typehint.TypehintRequestSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Api.Resource.Typehint.TypehintRequestJM ()
import Wizard.Database.Migration.Development.Typehint.Data.Typehints
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema TypehintRequestDTO where
  declareNamedSchema = toSwagger typehintRequest
