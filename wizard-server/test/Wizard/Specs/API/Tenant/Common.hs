module Wizard.Specs.API.Tenant.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Tenant.Tenant

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAppInDB appContext tenant = do
  tenantFromDb <- getOneFromDB (findTenantByUuid tenant.uuid) appContext
  compareTenantDtos tenantFromDb tenant

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTenantDtos resDto expDto = do
  liftIO $ resDto.tenantId `shouldBe` expDto.tenantId
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.serverDomain `shouldBe` expDto.serverDomain
  liftIO $ resDto.serverUrl `shouldBe` expDto.serverUrl
  liftIO $ resDto.clientUrl `shouldBe` expDto.clientUrl
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
