module Wizard.Specs.API.Tenant.Usage.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Usage.Current_Wizard_GET
import Wizard.Specs.API.Tenant.Usage.Detail_Wizard_GET

usageAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "USAGE API Spec" $ do
      current_wizard_GET appContext
      detail_wizard_GET appContext
