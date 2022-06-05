module Wizard.Specs.API.App.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.App.Detail_GET
import Wizard.Specs.API.App.Detail_PUT
import Wizard.Specs.API.App.List_GET
import Wizard.Specs.API.App.List_POST
import Wizard.Specs.API.Common

appAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
  describe "APP API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_GET appContext
    detail_PUT appContext
