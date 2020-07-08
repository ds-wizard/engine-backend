module Wizard.Specs.API.Package.Common where

import Control.Lens ((^.))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.PackageDAO

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPackageInDB appContext package = do
  packageFromDb <- getOneFromDB (findPackageById (package ^. pId)) appContext
  comparePackageDtos packageFromDb package

-- --------------------------------
-- COMPARATORS
-- --------------------------------
comparePackageDtos resDto expDto = do
  liftIO $ (resDto ^. pId) `shouldBe` (expDto ^. pId)
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (resDto ^. organizationId) `shouldBe` (expDto ^. organizationId)
  liftIO $ (resDto ^. kmId) `shouldBe` (expDto ^. kmId)
  liftIO $ (resDto ^. version) `shouldBe` (expDto ^. version)
  liftIO $ (resDto ^. description) `shouldBe` (expDto ^. description)
