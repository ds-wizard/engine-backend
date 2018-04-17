module Api.Handler.Branch.BranchHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Branch.BranchDTO
import Model.Context.AppContext
import Service.Branch.BranchService

getBranchesA :: Endpoint
getBranchesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    eitherDtos <- liftIO $ getBranches context
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postBranchesA :: Endpoint
postBranchesA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $
    getReqDto $ \reqDto -> do
      eitherResDto <- liftIO $ createBranch context reqDto
      case eitherResDto of
        Left appError -> sendError appError
        Right resDto -> do
          status created201
          json resDto

getBranchA :: Endpoint
getBranchA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherResDto <- liftIO $ getBranchById context branchUuid
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> json resDto

putBranchA :: Endpoint
putBranchA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      eitherResDto <- liftIO $ modifyBranch context branchUuid reqDto
      case eitherResDto of
        Left appError -> sendError appError
        Right resDto -> json resDto

deleteBranchA :: Endpoint
deleteBranchA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    maybeError <- liftIO $ deleteBranch context branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
