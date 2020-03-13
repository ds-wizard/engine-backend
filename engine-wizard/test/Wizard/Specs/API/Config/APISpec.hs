module Wizard.Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.List_Application_GET
import Wizard.Specs.API.Config.List_Application_PUT
import Wizard.Specs.API.Config.List_Client_GET

configAPI appContext =
  with (startWebApp appContext) $
  describe "CONFIG API Spec" $ do
    list_client_GET appContext
    list_application_GET appContext
    list_application_PUT appContext
