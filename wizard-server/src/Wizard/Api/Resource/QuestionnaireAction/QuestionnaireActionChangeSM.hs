module Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeJM ()
import Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions
import Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper

instance ToSchema QuestionnaireActionChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO questionnaireActionFtp1)
