module Wizard.Api.Resource.TypeHint.TypeHintRequestSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintRequestJM ()
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema TypeHintRequestDTO where
  declareNamedSchema = toSwagger typeHintRequest
