module Wizard.Specs.API.App.Plan.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Prelude hiding (until)

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Plan.AppPlanDAO
import Wizard.Model.Plan.AppPlan

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPlanInDB appContext plan = do
  planFromDb <- getFirstFromDB (findAppPlansForAppUuid plan.appUuid) appContext
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
