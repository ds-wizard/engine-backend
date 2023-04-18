module Wizard.Specs.API.Migration.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Migration.Questionnaire.List_Current_Completion_POST
import Wizard.Specs.API.Migration.Questionnaire.List_Current_DELETE
import Wizard.Specs.API.Migration.Questionnaire.List_Current_GET
import Wizard.Specs.API.Migration.Questionnaire.List_Current_PUT
import Wizard.Specs.API.Migration.Questionnaire.List_POST

migrationAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "MIGRATION QUESTIONNAIRE API Spec" $ do
      list_POST appContext
      list_current_GET appContext
      list_current_PUT appContext
      list_current_DELETE appContext
      list_current_completion_POST appContext
