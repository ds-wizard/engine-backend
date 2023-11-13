module Wizard.Api.Resource.Questionnaire.QuestionnairePermSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Acl.MemberSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnairePermType

instance ToSchema QuestionnairePerm where
  declareNamedSchema = toSwagger bioGroupEditQtnPerm

instance ToSchema QuestionnairePermDTO where
  declareNamedSchema =
    toSwagger (toUserQuestionnairePermDTO bioGroupEditQtnPerm userAlbert)
