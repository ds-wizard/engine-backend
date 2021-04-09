module Registry.Specs.API.Organization.Common where

import Control.Lens ((^.))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig
import Registry.Database.DAO.Organization.OrganizationSqlDAO

import Registry.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfOrganizationInDB appContext organization = do
  organizationFromDb <- getOneFromDB (findOrganizationByOrgId (organization ^. organizationId)) appContext
  compareOrganizationDtos organizationFromDb organization

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareOrganizationDtosWhenCreate resDto expDto = do
  liftIO $ resDto ^. organizationId `shouldBe` expDto ^. organizationId
  liftIO $ resDto ^. name `shouldBe` expDto ^. name
  liftIO $ resDto ^. description `shouldBe` expDto ^. description
  liftIO $ resDto ^. email `shouldBe` expDto ^. email

compareOrganizationDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareActionKeyDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
