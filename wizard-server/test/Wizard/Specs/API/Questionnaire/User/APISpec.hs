module Wizard.Specs.API.Questionnaire.User.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.User.List_Suggestions_GET

questionnaireUserAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "QUESTIONNAIRE USER API Spec" $
      list_suggestions_GET appContext
