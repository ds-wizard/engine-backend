module Wizard.Specs.API.Questionnaire.Event.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Event.Detail_GET
import Wizard.Specs.API.Questionnaire.Event.List_GET

questionnaireEventAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "QUESTIONNAIRE EVENT API Spec" $ do
      list_GET appContext
      detail_GET appContext
