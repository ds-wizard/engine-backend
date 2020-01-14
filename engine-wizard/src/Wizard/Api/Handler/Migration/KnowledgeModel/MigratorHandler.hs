module Wizard.Api.Handler.Migration.KnowledgeModel.MigratorHandler where

import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM ()
import Wizard.Service.Migration.KnowledgeModel.MigratorService

getMigrationsCurrentA :: Endpoint
getMigrationsCurrentA =
  checkPermission "KM_UPGRADE_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    branchUuid <- param "branchUuid"
    eitherDto <- runInAuthService $ getCurrentMigrationDto branchUuid
    case eitherDto of
      Right resDto -> json resDto
      Left error -> sendError error

postMigrationsCurrentA :: Endpoint
postMigrationsCurrentA =
  checkPermission "KM_UPGRADE_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      eitherResDto <- runInAuthService $ createMigration branchUuid reqDto
      case eitherResDto of
        Right resDto -> do
          status created201
          json resDto
        Left error -> sendError error

deleteMigrationsCurrentA :: Endpoint
deleteMigrationsCurrentA =
  checkPermission "KM_UPGRADE_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    branchUuid <- param "branchUuid"
    maybeError <- runInAuthService $ deleteCurrentMigration branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error

postMigrationsCurrentConflictA :: Endpoint
postMigrationsCurrentConflictA =
  checkPermission "KM_UPGRADE_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      maybeError <- runInAuthService $ solveConflictAndMigrate branchUuid reqDto
      case maybeError of
        Nothing -> status noContent204
        Just error -> sendError error
