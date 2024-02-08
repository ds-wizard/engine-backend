module Wizard.Specs.API.AppKey.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.AppKey.Detail_DELETE
import Wizard.Specs.API.AppKey.List_GET
import Wizard.Specs.API.AppKey.List_POST
import Wizard.Specs.API.Common

appKeyAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "APP KEY API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_DELETE appContext
