module Wizard.Specs.API.DocumentTemplateDraft.File.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.File.Detail_DELETE
import Wizard.Specs.API.DocumentTemplateDraft.File.Detail_GET
import Wizard.Specs.API.DocumentTemplateDraft.File.Detail_PUT
import Wizard.Specs.API.DocumentTemplateDraft.File.List_GET
import Wizard.Specs.API.DocumentTemplateDraft.File.List_POST

documentTemplateDraftFileAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "DOCUMENT TEMPLATE DRAFT FILE API Spec" $ do
      list_get appContext
      list_post appContext
      detail_get appContext
      detail_put appContext
      detail_delete appContext
