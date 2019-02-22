module Specs.API.Branch.Common where

import Control.Lens ((^.))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Branch.BranchDAO
import LensesConfig

import Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfBranchInDB appContext branch bParentPackageId bLastAppliedParentPackageId bOwnerUuid = do
  branchFromDb <- getFirstFromDB findBranches appContext
  compareBranchDtos branchFromDb branch bParentPackageId bLastAppliedParentPackageId bOwnerUuid

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareBranchDtos resDto expDto bParentPackageId bLastAppliedParentPackageId bOwnerUuid = do
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (resDto ^. kmId) `shouldBe` (expDto ^. kmId)
  liftIO $ (resDto ^. parentPackageId) `shouldBe` bParentPackageId
  liftIO $ (resDto ^. lastAppliedParentPackageId) `shouldBe` bLastAppliedParentPackageId
  liftIO $ (resDto ^. ownerUuid) `shouldBe` bOwnerUuid
