module Specs.API.Organization.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorJM ()
import Database.DAO.Organization.OrganizationDAO

import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfOrganizationInDB appContext organization = do
  eitherOrganization <- runInContextIO findOrganization appContext
  liftIO $ (isRight eitherOrganization) `shouldBe` True
  let (Right organizationFromDb) = eitherOrganization
  compareOrganizationDtos organizationFromDb organization

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareOrganizationDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
