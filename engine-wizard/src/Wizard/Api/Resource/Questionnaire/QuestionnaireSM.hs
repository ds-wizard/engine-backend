module Wizard.Api.Resource.Questionnaire.QuestionnaireSM where

import Data.Swagger

import Shared.Database.Migration.Development.Package.Data.Packages
import qualified Shared.Service.Package.PackageMapper as PM
import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilitySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import qualified Wizard.Service.User.UserMapper as UM

instance ToSchema QuestionnaireDTO where
  declareNamedSchema =
    simpleToSchema (toDTO questionnaire1 (PM.toPackage germanyPackage) QSDefault (Just . UM.toDTO $ userAlbert))
