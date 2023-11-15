module Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionJM ()
import Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions
import Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper

instance ToSchema QuestionnaireActionDTO where
  declareNamedSchema = toSwagger (toDTO questionnaireActionFtp1)
