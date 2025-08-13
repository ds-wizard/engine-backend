module Wizard.Api.Resource.TypeHint.TypeHintRequestSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintRequestJM ()
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema TypeHintLegacyRequestDTO where
  declareNamedSchema = toSwagger typeHintLegacyRequest

instance ToSchema TypeHintRequestDTO where
  declareNamedSchema = toSwaggerWithType "requestType" questionnaireTypeHintRequest

instance ToSchema BranchIntegrationTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" branchIntegrationTypeHintRequest'

instance ToSchema BranchQuestionTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" branchQuestionTypeHintRequest'

instance ToSchema QuestionnaireTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" questionnaireTypeHintRequest'
