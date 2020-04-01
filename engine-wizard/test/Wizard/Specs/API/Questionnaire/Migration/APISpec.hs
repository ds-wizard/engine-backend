module Wizard.Specs.API.Questionnaire.Migration.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Migration.List_Current_Completion_POST
import Wizard.Specs.API.Questionnaire.Migration.List_Current_DELETE
import Wizard.Specs.API.Questionnaire.Migration.List_Current_GET
import Wizard.Specs.API.Questionnaire.Migration.List_Current_PUT
import Wizard.Specs.API.Questionnaire.Migration.List_POST

questionnaireMigrationAPI appContext =
  with (startWebApp appContext) $
  describe "QUESTIONNAIRE MIGRATION API Spec" $ do
    list_POST appContext
    list_current_GET appContext
    list_current_PUT appContext
    list_current_DELETE appContext
    list_current_completion_POST appContext
