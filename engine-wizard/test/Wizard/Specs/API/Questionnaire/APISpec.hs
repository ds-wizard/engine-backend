module Wizard.Specs.API.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Detail_Content_PUT
import Wizard.Specs.API.Questionnaire.Detail_DELETE
import Wizard.Specs.API.Questionnaire.Detail_Documents_GET
import Wizard.Specs.API.Questionnaire.Detail_GET
import Wizard.Specs.API.Questionnaire.Detail_PUT
import Wizard.Specs.API.Questionnaire.Detail_Report_GET
import Wizard.Specs.API.Questionnaire.Detail_Revert_POST
import Wizard.Specs.API.Questionnaire.Detail_Revert_Preview_POST
import Wizard.Specs.API.Questionnaire.List_GET
import Wizard.Specs.API.Questionnaire.List_POST
import Wizard.Specs.API.Questionnaire.List_POST_CloneUuid

questionnaireAPI appContext =
  with (startWebApp appContext) $
  describe "QUESTIONNAIRE API Spec" $ do
    list_get appContext
    list_post appContext
    list_post_cloneUuid appContext
    detail_get appContext
    detail_put appContext
    detail_delete appContext
    detail_content_put appContext
    detail_report_get appContext
    detail_documents_get appContext
    detail_revert_POST appContext
    detail_revert_preview_POST appContext
