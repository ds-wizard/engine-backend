module Wizard.Specs.API.Tenant.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Detail_GET
import Wizard.Specs.API.Tenant.Detail_PUT
import Wizard.Specs.API.Tenant.List_GET
import Wizard.Specs.API.Tenant.List_POST
import Wizard.Specs.API.Tenant.PluginSettings.APISpec

tenantAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TENANT API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      tenantPluginSettingsAPI appContext
