module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadAssignedSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadAssignedJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireCommentThreadAssigned where
  declareNamedSchema = toSwagger cmtAssigned
