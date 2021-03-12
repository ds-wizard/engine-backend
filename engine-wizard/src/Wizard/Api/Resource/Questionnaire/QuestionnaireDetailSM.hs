module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Package.Data.Packages
import qualified Shared.Service.Package.PackageMapper as PM
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireDetailDTO where
  declareNamedSchema =
    simpleToSchema
      (toDetailWithPackageWithEventsDTO
         questionnaire1
         questionnaire1Ctn
         (PM.toPackage germanyPackage)
         ["1.0.0"]
         km1WithQ4
         QSDefault
         Nothing
         Nothing
         fReplies
         []
         []
         [])
