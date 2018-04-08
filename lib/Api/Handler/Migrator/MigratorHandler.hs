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
import Api.Resource.Migrator.MigratorStateCreateDTO
import Api.Resource.Migrator.MigratorStateDTO
import Common.Context
import Model.Config.DSWConfig
import Service.Migrator.MigratorService

getMigrationsCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
getMigrationsCurrentA context dswConfig = do
  checkPermission context "KM_UPGRADE_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    eitherDto <- liftIO $ getCurrentMigration context branchUuid
    case eitherDto of
      Right resDto -> sendJson resDto
      Left error -> sendError error

postMigrationsCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
postMigrationsCurrentA context dswConfig =
  checkPermission context "KM_UPGRADE_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- Scotty.param "branchUuid"
    eitherResDto <- liftIO $ createMigration context branchUuid reqDto
    case eitherResDto of
      Right resDto -> do
        Scotty.status created201
        sendJson resDto
      Left error -> sendError error

deleteMigrationsCurrentA :: Context -> DSWConfig -> Scotty.ActionM ()
deleteMigrationsCurrentA context dswConfig =
  checkPermission context "KM_UPGRADE_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    maybeError <- liftIO $ deleteCurrentMigration context branchUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error

postMigrationsCurrentConflictA :: Context -> DSWConfig -> Scotty.ActionM ()
postMigrationsCurrentConflictA context dswConfig =
  checkPermission context "KM_UPGRADE_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- Scotty.param "branchUuid"
    maybeError <- liftIO $ solveConflictAndMigrate context branchUuid reqDto
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
