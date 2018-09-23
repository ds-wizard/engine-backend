module Specs.API.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Questionnaire.Detail_GET
import Specs.API.Questionnaire.Detail_PUT
import Specs.API.Questionnaire.Detail_Report_GET
import Specs.API.Questionnaire.Detail_Report_Preview_POST
import Specs.API.Questionnaire.List_GET
import Specs.API.Questionnaire.Public_GET

questionnaireAPI appContext =
  with (startWebApp appContext) $
  describe "QUESTIONNAIRE API Spec" $ do
    list_get appContext
    public_get appContext
    detail_get appContext
    detail_put appContext
    detail_report_get appContext
    detail_report_preview_post appContext
