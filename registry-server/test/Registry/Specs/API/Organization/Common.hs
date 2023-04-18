module Registry.Specs.API.Organization.Common where

import Test.Hspec
import Test.Hspec.Wai

import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Organization.Organization

import Registry.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfOrganizationInDB appContext organization = do
  organizationFromDb <- getOneFromDB (findOrganizationByOrgId organization.organizationId) appContext
  compareOrganizationDtos organizationFromDb organization

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareOrganizationDtosWhenCreate resDto expDto = do
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.email `shouldBe` expDto.email

compareOrganizationDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
