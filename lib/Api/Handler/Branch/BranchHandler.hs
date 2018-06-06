module Api.Handler.Branch.BranchHandler where

import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Service.Branch.BranchService

getBranchesA :: Endpoint
getBranchesA =
  checkPermission "KM_PERM" $ do
    eitherDtos <- lift getBranches
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postBranchesA :: Endpoint
postBranchesA =
  checkPermission "KM_PERM" $
  getReqDto $ \reqDto -> do
    eitherResDto <- lift $ createBranch reqDto
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> do
        status created201
        json resDto

getBranchA :: Endpoint
getBranchA =
  checkPermission "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherResDto <- lift $ getBranchById branchUuid
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> json resDto

putBranchA :: Endpoint
putBranchA =
  checkPermission "KM_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- param "branchUuid"
    eitherResDto <- lift $ modifyBranch branchUuid reqDto
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> json resDto

deleteBranchA :: Endpoint
deleteBranchA =
  checkPermission "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    maybeError <- lift $ deleteBranch branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
