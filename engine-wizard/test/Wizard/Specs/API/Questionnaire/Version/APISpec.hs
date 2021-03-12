module Wizard.Specs.API.Questionnaire.Version.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Version.Detail_DELETE
import Wizard.Specs.API.Questionnaire.Version.Detail_PUT
import Wizard.Specs.API.Questionnaire.Version.List_GET
import Wizard.Specs.API.Questionnaire.Version.List_POST

questionnaireVersionAPI appContext =
  with (startWebApp appContext) $
  describe "QUESTIONNAIRE VERSION API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_PUT appContext
    detail_delete appContext
