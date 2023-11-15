module Wizard.Specs.API.QuestionnaireAction.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.QuestionnaireAction.Detail_GET
import Wizard.Specs.API.QuestionnaireAction.Detail_PUT
import Wizard.Specs.API.QuestionnaireAction.List_GET
import Wizard.Specs.API.QuestionnaireAction.List_Suggestions_GET

questionnaireActionAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "QUESTIONNAIRE ACTION API Spec" $ do
      list_GET appContext
      list_suggestions_GET appContext
      detail_GET appContext
      detail_PUT appContext
