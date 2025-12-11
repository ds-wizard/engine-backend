module Wizard.Specs.API.Project.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Project.Comment.APISpec
import Wizard.Specs.API.Project.Detail_Content_PUT
import Wizard.Specs.API.Project.Detail_DELETE
import Wizard.Specs.API.Project.Detail_Documents_GET
import Wizard.Specs.API.Project.Detail_GET
import Wizard.Specs.API.Project.Detail_Preview_GET
import Wizard.Specs.API.Project.Detail_Questionnaire_GET
import Wizard.Specs.API.Project.Detail_Report_GET
import Wizard.Specs.API.Project.Detail_Revert_POST
import Wizard.Specs.API.Project.Detail_Revert_Preview_POST
import Wizard.Specs.API.Project.Detail_Settings_GET
import Wizard.Specs.API.Project.Detail_Settings_PUT
import Wizard.Specs.API.Project.Detail_Share_PUT
import Wizard.Specs.API.Project.Event.APISpec
import Wizard.Specs.API.Project.List_GET
import Wizard.Specs.API.Project.List_POST
import Wizard.Specs.API.Project.List_POST_CloneUuid
import Wizard.Specs.API.Project.List_POST_FromTemplate
import Wizard.Specs.API.Project.Migration.APISpec
import Wizard.Specs.API.Project.ProjectTag.APISpec
import Wizard.Specs.API.Project.User.APISpec
import Wizard.Specs.API.Project.Version.APISpec

projectAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "PROJECT API Spec" $ do
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
      projectCommentAPI appContext
      projectEventAPI appContext
      projectMigrationAPI appContext
      projectTagAPI appContext
      projectUserAPI appContext
      projectVersionAPI appContext
