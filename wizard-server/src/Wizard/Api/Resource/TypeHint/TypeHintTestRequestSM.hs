module Wizard.Api.Resource.TypeHint.TypeHintTestRequestSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestJM ()
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema TypeHintTestRequestDTO where
  declareNamedSchema = toSwagger typeHintTestRequest
