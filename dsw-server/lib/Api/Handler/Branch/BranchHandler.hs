module Api.Handler.Branch.BranchHandler where

import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Branch.BranchChangeJM ()
import Api.Resource.Branch.BranchCreateJM ()
import Api.Resource.Branch.BranchDetailJM ()
import Api.Resource.Branch.BranchJM ()
import Service.Branch.BranchService

getBranchesA :: Endpoint
getBranchesA =
  checkPermission "KM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDtos <- runInAuthService getBranches
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postBranchesA :: Endpoint
postBranchesA =
  checkPermission "KM_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherResDto <- runInAuthService $ createBranch reqDto
      case eitherResDto of
        Left appError -> sendError appError
        Right resDto -> do
          status created201
          json resDto

getBranchA :: Endpoint
getBranchA =
  checkPermission "KM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    branchUuid <- param "branchUuid"
    eitherResDto <- runInAuthService $ getBranchById branchUuid
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> json resDto

putBranchA :: Endpoint
putBranchA =
  checkPermission "KM_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      eitherResDto <- runInAuthService $ modifyBranch branchUuid reqDto
      case eitherResDto of
        Left appError -> sendError appError
        Right resDto -> json resDto

deleteBranchA :: Endpoint
deleteBranchA =
  checkPermission "KM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    branchUuid <- param "branchUuid"
    maybeError <- runInAuthService $ deleteBranch branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
