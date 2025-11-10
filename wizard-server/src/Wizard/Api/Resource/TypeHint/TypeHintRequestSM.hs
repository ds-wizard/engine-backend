module Wizard.Api.Resource.TypeHint.TypeHintRequestSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintRequestJM ()
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints

instance ToSchema TypeHintLegacyRequestDTO where
  declareNamedSchema = toSwagger typeHintLegacyRequest

instance ToSchema TypeHintRequestDTO where
  declareNamedSchema = toSwaggerWithType "requestType" questionnaireTypeHintRequest

instance ToSchema KnowledgeModelEditorIntegrationTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" kmEditorIntegrationTypeHintRequest'

instance ToSchema KnowledgeModelEditorQuestionTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" kmEditorQuestionTypeHintRequest'

instance ToSchema QuestionnaireTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" questionnaireTypeHintRequest'
