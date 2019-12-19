module Registry.Specs.API.Organization.Common where

import Control.Lens ((^.))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.LensesConfig
import Shared.Api.Resource.Error.ErrorDTO ()

import Registry.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfOrganizationInDB appContext organization = do
  organizationFromDb <- getFirstFromDB findOrganizations appContext
  compareOrganizationDtos organizationFromDb organization

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareOrganizationDtosWhenCreate resDto expDto = do
  liftIO $ (resDto ^. organizationId == expDto ^. organizationId) `shouldBe` True
  liftIO $ (resDto ^. name == expDto ^. name) `shouldBe` True
  liftIO $ (resDto ^. description == expDto ^. description) `shouldBe` True
  liftIO $ (resDto ^. email == expDto ^. email) `shouldBe` True

compareOrganizationDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True

compareActionKeyDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
