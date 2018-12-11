module Specs.API.Branch.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Branch.BranchDAO
import LensesConfig

import Specs.Common

assertCountOfBranchesInDB appContext count = do
  eitherBranchesFromDb <- runInContextIO findBranches appContext
  liftIO $ (isRight eitherBranchesFromDb) `shouldBe` True
  let (Right branchesFromDb) = eitherBranchesFromDb
  liftIO $ (length branchesFromDb) `shouldBe` count

assertExistenceOfBranchInDB appContext branch bLastAppliedParentPackageId bOwnerUuid = do
  eitherBranchesFromDb <- runInContextIO findBranches appContext
  liftIO $ (isRight eitherBranchesFromDb) `shouldBe` True
  let (Right branchesFromDb) = eitherBranchesFromDb
  let branchFromDb = branchesFromDb !! 0
  compareBranchDtos branchFromDb branch bLastAppliedParentPackageId bOwnerUuid

compareBranchDtos resDto expDto bLastAppliedParentPackageId bOwnerUuid = do
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (resDto ^. kmId) `shouldBe` (expDto ^. kmId)
  liftIO $ (resDto ^. parentPackageId) `shouldBe` (expDto ^. parentPackageId)
  liftIO $ (resDto ^. lastAppliedParentPackageId) `shouldBe` bLastAppliedParentPackageId
  liftIO $ (resDto ^. ownerUuid) `shouldBe` bOwnerUuid
