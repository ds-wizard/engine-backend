module Wizard.Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.List_App_GET
import Wizard.Specs.API.Config.List_App_PUT
import Wizard.Specs.API.Config.List_Bootstrap_GET

configAPI appContext =
  with (startWebApp appContext) $
  describe "CONFIG API Spec" $ do
    list_app_GET appContext
    list_app_PUT appContext
    list_bootstrap_GET appContext
