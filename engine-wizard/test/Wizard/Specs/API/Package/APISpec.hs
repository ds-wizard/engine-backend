module Wizard.Specs.API.Package.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Package.Detail_DELETE
import Wizard.Specs.API.Package.Detail_GET
import Wizard.Specs.API.Package.Detail_Pull_POST
import Wizard.Specs.API.Package.List_DELETE
import Wizard.Specs.API.Package.List_GET
import Wizard.Specs.API.Package.List_POST

packageAPI appContext =
  with (startWebApp appContext) $
  describe "PACKAGE API Spec" $ do
    list_get appContext
    list_post appContext
    list_delete appContext
    detail_get appContext
    detail_delete appContext
    detail_pull_post appContext
