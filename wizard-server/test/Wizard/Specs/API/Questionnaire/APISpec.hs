module Wizard.Specs.API.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Detail_Content_PUT
import Wizard.Specs.API.Questionnaire.Detail_DELETE
import Wizard.Specs.API.Questionnaire.Detail_Documents_GET
import Wizard.Specs.API.Questionnaire.Detail_GET
import Wizard.Specs.API.Questionnaire.Detail_Preview_GET
import Wizard.Specs.API.Questionnaire.Detail_Questionnaire_GET
import Wizard.Specs.API.Questionnaire.Detail_Report_GET
import Wizard.Specs.API.Questionnaire.Detail_Revert_POST
import Wizard.Specs.API.Questionnaire.Detail_Revert_Preview_POST
import Wizard.Specs.API.Questionnaire.Detail_Settings_GET
import Wizard.Specs.API.Questionnaire.Detail_Settings_PUT
import Wizard.Specs.API.Questionnaire.Detail_Share_PUT
import Wizard.Specs.API.Questionnaire.List_GET
import Wizard.Specs.API.Questionnaire.List_POST
import Wizard.Specs.API.Questionnaire.List_POST_CloneUuid
import Wizard.Specs.API.Questionnaire.List_POST_FromTemplate

questionnaireAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "QUESTIONNAIRE API Spec" $ do
      list_GET appContext
      list_POST appContext
      list_POST_fromTemplate appContext
      list_POST_cloneUuid appContext
      detail_GET appContext
      detail_questionnaire_GET appContext
      detail_share_PUT appContext
      detail_preview_GET appContext
      detail_settings_GET appContext
      detail_settings_PUT appContext
      detail_DELETE appContext
      detail_content_PUT appContext
      detail_report_GET appContext
      detail_documents_GET appContext
      detail_revert_POST appContext
      detail_revert_preview_POST appContext
