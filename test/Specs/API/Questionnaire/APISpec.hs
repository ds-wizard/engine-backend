module Specs.API.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Questionnaire.Detail_Report_GET
import Specs.API.Questionnaire.Detail_Report_Preview_POST
import Specs.API.Questionnaire.Public_GET

questionnaireAPI appContext =
  with (startWebApp appContext) $
  describe "QUESTIONNAIRE API Spec" $ do
    public_get appContext
    detail_report_get appContext
    detail_report_preview_post appContext
