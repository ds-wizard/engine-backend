module Wizard.Api.Resource.Questionnaire.QuestionnaireSM where

import Data.Swagger

import Shared.Database.Migration.Development.Package.Data.Packages
import qualified Shared.Service.Package.PackageMapper as PM
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireDTO where
  declareNamedSchema =
    simpleToSchema
      (toDTO
         questionnaire1
         questionnaire1Ctn
         (PM.toPackage germanyPackage)
         QSDefault
         questionnaireReport
         [albertEditPermRecordDto])
