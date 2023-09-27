module Wizard.Specs.API.Tenant.Plan.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Prelude hiding (until)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Tenant.TenantPlanDAO
import Wizard.Model.Tenant.Plan.TenantPlan

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPlanInDB appContext plan = do
  planFromDb <- getFirstFromDB (findPlansForTenantUuid plan.tenantUuid) appContext
  comparePlanDtos planFromDb plan

-- --------------------------------
-- COMPARATORS
-- --------------------------------
comparePlanDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.users `shouldBe` expDto.users
  liftIO $ resDto.since `shouldBe` expDto.since
  liftIO $ resDto.until `shouldBe` expDto.until
  liftIO $ resDto.test `shouldBe` expDto.test
