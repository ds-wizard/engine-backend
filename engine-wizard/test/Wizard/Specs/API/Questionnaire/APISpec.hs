module Wizard.Specs.API.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Detail_DELETE
import Wizard.Specs.API.Questionnaire.Detail_GET
import Wizard.Specs.API.Questionnaire.Detail_PUT
import Wizard.Specs.API.Questionnaire.Detail_Report_GET
import Wizard.Specs.API.Questionnaire.Detail_Report_Preview_POST
import Wizard.Specs.API.Questionnaire.List_GET
import Wizard.Specs.API.Questionnaire.List_POST
import Wizard.Specs.API.Questionnaire.Public_GET

questionnaireAPI appContext =
  with (startWebApp appContext) $
  describe "QUESTIONNAIRE API Spec" $ do
    list_get appContext
    list_post appContext
    public_get appContext
    detail_get appContext
    detail_put appContext
    detail_delete appContext
    detail_report_get appContext
    detail_report_preview_post appContext
