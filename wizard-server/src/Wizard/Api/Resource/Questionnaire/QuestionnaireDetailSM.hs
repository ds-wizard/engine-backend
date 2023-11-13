module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PM

instance ToSchema QuestionnaireDetailDTO where
  declareNamedSchema =
    toSwagger
      ( toDetailWithPackageWithEventsDTO
          questionnaire1
          questionnaire1Ctn
          (PM.toPackage germanyPackage)
          km1WithQ4
          QSDefault
          Nothing
          Nothing
          fReplies
          M.empty
          []
          []
          Nothing
      )
