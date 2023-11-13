module Wizard.Api.Resource.Questionnaire.QuestionnaireSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PM

instance ToSchema QuestionnaireDTO where
  declareNamedSchema =
    toSwagger (toDTO questionnaire1 (PM.toPackage germanyPackage) QSDefault [qtn1AlbertEditQtnPermDto])
