module Wizard.Specs.API.DocumentTemplateDraft.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Detail_DELETE
import Wizard.Specs.API.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT
import Wizard.Specs.API.DocumentTemplateDraft.Detail_GET
import Wizard.Specs.API.DocumentTemplateDraft.Detail_PUT
import Wizard.Specs.API.DocumentTemplateDraft.List_GET
import Wizard.Specs.API.DocumentTemplateDraft.List_POST

documentTemplateDraftAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "DOCUMENT TEMPLATE DRAFT API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
      detail_documents_preview_settings_PUT appContext
