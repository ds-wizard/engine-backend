module Wizard.Specs.API.Tenant.Limit.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Limit.List_PUT

tenantLimitAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TENANT Limit API Spec" $
      list_PUT appContext
