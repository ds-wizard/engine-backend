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
assertExistenceOfTenantConfigInDB appContext tenantConfig = do
  eitherTenantConfig <- runInContextIO getCurrentTenantConfig appContext
  liftIO $ isRight eitherTenantConfig `shouldBe` True
  let (Right tenantConfigFromDb) = eitherTenantConfig
  compareDtos tenantConfigFromDb tenantConfig

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
