module Wizard.Specs.API.Template.File.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.File.Detail_DELETE
import Wizard.Specs.API.Template.File.Detail_GET
import Wizard.Specs.API.Template.File.Detail_PUT
import Wizard.Specs.API.Template.File.List_GET
import Wizard.Specs.API.Template.File.List_POST

templateFileAPI appContext =
  with (startWebApp appContext) $
  describe "TEMPLATE FILE API Spec" $ do
    list_get appContext
    list_post appContext
    detail_get appContext
    detail_put appContext
    detail_delete appContext
