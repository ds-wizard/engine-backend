module Wizard.Specs.API.Tenant.Plan.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Plan.Detail_DELETE
import Wizard.Specs.API.Tenant.Plan.Detail_PUT
import Wizard.Specs.API.Tenant.Plan.List_POST

tenantPlanAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "PLAN API Spec" $ do
      list_POST appContext
      detail_PUT appContext
      detail_DELETE appContext
