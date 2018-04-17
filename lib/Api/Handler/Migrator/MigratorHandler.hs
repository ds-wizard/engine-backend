module Api.Handler.Migrator.MigratorHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Migrator.MigratorStateCreateDTO
import Api.Resource.Migrator.MigratorStateDTO
import Model.Context.AppContext
import Service.Migrator.MigratorService

getMigrationsCurrentA :: Endpoint
getMigrationsCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_UPGRADE_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherDto <- liftIO $ getCurrentMigration context branchUuid
    case eitherDto of
      Right resDto -> json resDto
      Left error -> sendError error

postMigrationsCurrentA :: Endpoint
postMigrationsCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_UPGRADE_PERM" $
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      eitherResDto <- liftIO $ createMigration context branchUuid reqDto
      case eitherResDto of
        Right resDto -> do
          status created201
          json resDto
        Left error -> sendError error

deleteMigrationsCurrentA :: Endpoint
deleteMigrationsCurrentA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_UPGRADE_PERM" $ do
    branchUuid <- param "branchUuid"
    maybeError <- liftIO $ deleteCurrentMigration context branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

postMigrationsCurrentConflictA :: Endpoint
postMigrationsCurrentConflictA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_UPGRADE_PERM" $
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      maybeError <- liftIO $ solveConflictAndMigrate context branchUuid reqDto
      case maybeError of
        Nothing -> status noContent204
        Just error -> sendError error
