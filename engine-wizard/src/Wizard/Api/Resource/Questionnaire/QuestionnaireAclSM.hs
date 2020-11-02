module Wizard.Api.Resource.Questionnaire.QuestionnaireAclSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Acl.AclSM ()
import Wizard.Api.Resource.Acl.MemberSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnairePermRecord where
  declareNamedSchema = simpleToSchema' "_questionnairePermRecord" bioGroupEditPermRecord

instance ToSchema QuestionnairePermRecordDTO where
  declareNamedSchema =
    simpleToSchema' "_questionnairePermRecordDTO" (toUserPermRecordDTO bioGroupEditPermRecord userAlbert)
