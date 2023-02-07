module Wizard.Specs.API.DocumentTemplateDraft.Asset.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Asset.Detail_DELETE
import Wizard.Specs.API.DocumentTemplateDraft.Asset.Detail_GET
import Wizard.Specs.API.DocumentTemplateDraft.Asset.Detail_PUT
import Wizard.Specs.API.DocumentTemplateDraft.Asset.List_GET

documentTemplateDraftAssetAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "DOCUMENT TEMPLATE DRAFT ASSET API Spec" $ do
      list_GET appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
