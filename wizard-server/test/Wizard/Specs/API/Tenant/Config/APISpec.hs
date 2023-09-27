module Wizard.Specs.API.Tenant.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Config.List_GET
import Wizard.Specs.API.Tenant.Config.List_PUT

tenantConfigAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TENANT CONFIG API Spec" $ do
      list_GET appContext
      list_PUT appContext
