module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireChangeDTO where
  declareNamedSchema = toSwagger questionnaire1EditedChange
