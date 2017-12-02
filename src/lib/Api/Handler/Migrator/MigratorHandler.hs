module Api.Handler.Migrator.MigratorHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.Migrator.MigratorStateCreateDTO
import Api.Resources.Migrator.MigratorStateDTO
import Common.Context
import Common.DSPConfig
import Service.Migrator.MigratorService

getMigrationsCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
getMigrationsCurrentA context dspConfig = do
  checkPermission context "KM_UPGRADE_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    eitherDto <- liftIO $ getCurrentMigration context branchUuid
    case eitherDto of
      Right resDto -> sendJson resDto
      Left error -> sendError error

postMigrationsCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
postMigrationsCurrentA context dspConfig =
  checkPermission context "KM_UPGRADE_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- Scotty.param "branchUuid"
    eitherResDto <- liftIO $ createMigration context branchUuid reqDto
    case eitherResDto of
      Right resDto -> do
        Scotty.status created201
        sendJson resDto
      Left error -> sendError error

deleteMigrationsCurrentA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteMigrationsCurrentA context dspConfig =
  checkPermission context "KM_UPGRADE_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    maybeError <- liftIO $ deleteCurrentMigration context branchUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error

postMigrationsCurrentConflictA :: Context -> DSPConfig -> Scotty.ActionM ()
postMigrationsCurrentConflictA context dspConfig =
  checkPermission context "KM_UPGRADE_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- Scotty.param "branchUuid"
    maybeError <- liftIO $ solveConflict context branchUuid reqDto
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
