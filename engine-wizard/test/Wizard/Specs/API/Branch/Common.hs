module Wizard.Specs.API.Branch.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Model.Branch.Branch

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfBranchInDB appContext branch bPreviousPackageId bForkOfPackageId bCreatedBy = do
  branchFromDb <- getFirstFromDB findBranches appContext
  compareBranch branchFromDb branch bPreviousPackageId bForkOfPackageId bCreatedBy

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareBranch resDto expDto bPreviousPackageId bForkOfPackageId bCreatedBy = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.previousPackageId `shouldBe` bPreviousPackageId
  liftIO $ resDto.createdBy `shouldBe` bCreatedBy

compareBranchDtos resDto expDto bPreviousPackageId bForkOfPackageId bCreatedBy = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.readme `shouldBe` expDto.readme
  liftIO $ resDto.license `shouldBe` expDto.license
  liftIO $ resDto.previousPackageId `shouldBe` bPreviousPackageId
  liftIO $ resDto.createdBy `shouldBe` bCreatedBy
