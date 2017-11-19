module Api.Handler.Branch.BranchHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.Branch.BranchDTO
import Common.Context
import Common.DSPConfig
import Service.Branch.BranchService

getBranchesA :: Context -> DSPConfig -> Scotty.ActionM ()
getBranchesA context dspConfig =
  checkPermission context "KM_PERM" $ do
    eitherDtos <- liftIO $ getBranches context
    case eitherDtos of
      Right dtos -> sendJson dtos
      Left error -> sendError error

postBranchesA :: Context -> DSPConfig -> Scotty.ActionM ()
postBranchesA context dspConfig =
  checkPermission context "KM_PERM" $
  getReqDto $ \reqDto -> do
    eitherResDto <- liftIO $ createBranch context reqDto
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> do
        Scotty.status created201
        sendJson resDto

getBranchA :: Context -> DSPConfig -> Scotty.ActionM ()
getBranchA context dspConfig =
  checkPermission context "KM_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    eitherResDto <- liftIO $ getBranchById context branchUuid
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> sendJson resDto

putBranchA :: Context -> DSPConfig -> Scotty.ActionM ()
putBranchA context dspConfig =
  checkPermission context "KM_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- Scotty.param "branchUuid"
    eitherResDto <-
      liftIO $ modifyBranch context branchUuid reqDto
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> sendJson resDto

deleteBranchA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteBranchA context dspConfig =
  checkPermission context "KM_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    maybeError <- liftIO $ deleteBranch context branchUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
