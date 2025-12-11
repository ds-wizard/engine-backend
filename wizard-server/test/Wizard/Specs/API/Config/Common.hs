module Wizard.Specs.API.Config.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Service.Tenant.Config.ConfigService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTenantConfigProjectInDB appContext tcProject = do
  eitherTcProject <- runInContextIO getCurrentTenantConfigProject appContext
  liftIO $ isRight eitherTcProject `shouldBe` True
  let (Right tcProjectFromDb) = eitherTcProject
  compareDtos tcProjectFromDb tcProject

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
