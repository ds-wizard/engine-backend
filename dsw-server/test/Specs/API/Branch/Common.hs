module Specs.API.Branch.Common where

import Control.Lens ((^.))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorJM ()
import Database.DAO.Branch.BranchDAO
import LensesConfig

import Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfBranchInDB appContext branch bPreviousPackageId bForkOfPackageId bOwnerUuid = do
  branchFromDb <- getFirstFromDB findBranches appContext
  compareBranchDtos branchFromDb branch bPreviousPackageId bForkOfPackageId bOwnerUuid

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareBranchDtos resDto expDto bPreviousPackageId bForkOfPackageId bOwnerUuid = do
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (resDto ^. kmId) `shouldBe` (expDto ^. kmId)
  liftIO $ (resDto ^. previousPackageId) `shouldBe` bPreviousPackageId
  liftIO $ (resDto ^. ownerUuid) `shouldBe` bOwnerUuid
