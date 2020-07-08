module Wizard.Specs.API.Template.Asset.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.Asset.Detail_DELETE
import Wizard.Specs.API.Template.Asset.Detail_GET
import Wizard.Specs.API.Template.Asset.List_GET

templateAssetAPI appContext =
  with (startWebApp appContext) $
  describe "TEMPLATE ASSET API Spec" $ do
    list_get appContext
    detail_get appContext
    detail_delete appContext
