module Api.Handler.Migrator.MigratorHandler where

import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Migrator.MigratorConflictJM ()
import Api.Resource.Migrator.MigratorStateCreateJM ()
import Api.Resource.Migrator.MigratorStateJM ()
import Service.Migrator.MigratorService

getMigrationsCurrentA :: Endpoint
getMigrationsCurrentA =
  checkPermission "KM_UPGRADE_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherDto <- lift $ getCurrentMigration branchUuid
    case eitherDto of
      Right resDto -> json resDto
      Left error -> sendError error

postMigrationsCurrentA :: Endpoint
postMigrationsCurrentA =
  checkPermission "KM_UPGRADE_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- param "branchUuid"
    eitherResDto <- lift $ createMigration branchUuid reqDto
    case eitherResDto of
      Right resDto -> do
        status created201
        json resDto
      Left error -> sendError error

deleteMigrationsCurrentA :: Endpoint
deleteMigrationsCurrentA =
  checkPermission "KM_UPGRADE_PERM" $ do
    branchUuid <- param "branchUuid"
    maybeError <- lift $ deleteCurrentMigration branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

postMigrationsCurrentConflictA :: Endpoint
postMigrationsCurrentConflictA =
  checkPermission "KM_UPGRADE_PERM" $
  getReqDto $ \reqDto -> do
    branchUuid <- param "branchUuid"
    maybeError <- lift $ solveConflictAndMigrate branchUuid reqDto
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
