module Wizard.Service.Branch.BranchService where

import Control.Lens ((^.))
import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchMapper
import Wizard.Service.Branch.BranchUtils
import Wizard.Service.Branch.BranchValidation
import Wizard.Service.Common.ACL
import Wizard.Service.Package.PackageService

getBranches :: AppContextM [BranchDTO]
getBranches = do
  checkPermission _KM_PERM
  bs <- findBranchesWithEvents
  forM bs enhance
  where
    enhance :: BranchWithEvents -> AppContextM BranchDTO
    enhance branch = do
      mForkOfPackageId <- getBranchForkOfPackageId branch
      state <- getBranchState branch
      return $ toDTO branch mForkOfPackageId state

createBranch :: BranchCreateDTO -> AppContextM BranchDTO
createBranch reqDto = do
  bUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  currentUser <- getCurrentUser
  createBranchWithParams bUuid now currentUser reqDto

createBranchWithParams :: U.UUID -> UTCTime -> UserDTO -> BranchCreateDTO -> AppContextM BranchDTO
createBranchWithParams bUuid now currentUser reqDto = do
  checkPermission _KM_PERM
  validateNewKmId (reqDto ^. kmId)
  validatePackageExistence (reqDto ^. previousPackageId)
  let branch = fromCreateDTO reqDto bUuid (Just $ currentUser ^. uuid) now now
  insertBranch branch
  createDefaultEventIfPreviousPackageIsNotPresent branch
  return $ toDTO branch Nothing BSDefault
  where
    createDefaultEventIfPreviousPackageIsNotPresent branch = do
      let branchUuid = U.toString $ branch ^. uuid
      let mPreviousPackageId = branch ^. previousPackageId
      case mPreviousPackageId of
        Just _ -> return ()
        Nothing -> do
          uuid <- liftIO generateUuid
          kmUuid <- liftIO generateUuid
          let addKMEvent =
                AddKnowledgeModelEvent
                  { _addKnowledgeModelEventUuid = uuid
                  , _addKnowledgeModelEventParentUuid = U.nil
                  , _addKnowledgeModelEventEntityUuid = kmUuid
                  , _addKnowledgeModelEventName = "New knowledge model"
                  }
          updateEventsInBranch branchUuid [AddKnowledgeModelEvent' addKMEvent]

getBranchById :: String -> AppContextM BranchDetailDTO
getBranchById branchUuid = do
  checkPermission _KM_PERM
  branch <- findBranchWithEventsById branchUuid
  mForkOfPackageId <- getBranchForkOfPackageId branch
  branchState <- getBranchState branch
  return $ toDetailDTO branch mForkOfPackageId branchState

modifyBranch :: String -> BranchChangeDTO -> AppContextM BranchDetailDTO
modifyBranch branchUuid reqDto = do
  checkPermission _KM_PERM
  branchFromDB <- findBranchById branchUuid
  validateKmId
  now <- liftIO getCurrentTime
  let branch =
        fromChangeDTO
          reqDto
          (branchFromDB ^. uuid)
          (branchFromDB ^. metamodelVersion)
          (branchFromDB ^. previousPackageId)
          (branchFromDB ^. ownerUuid)
          (branchFromDB ^. createdAt)
          now
  updateBranchById branch
  mForkOfPackageId <- getBranchForkOfPackageId branch
  branchState <- getBranchState branch
  return $ toDetailDTO branch mForkOfPackageId branchState
  where
    validateKmId = do
      let bKmId = reqDto ^. kmId
      case isValidKmId bKmId of
        Nothing -> do
          branch <- findBranchById branchUuid
          mBranchFromDb <- findBranchByKmId' bKmId
          when
            (isAlreadyUsedAndIsNotMine mBranchFromDb)
            (throwError $ ValidationError [] [("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS bKmId)])
        Just error -> throwError error
    isAlreadyUsedAndIsNotMine (Just branch) = U.toString (branch ^. uuid) /= branchUuid
    isAlreadyUsedAndIsNotMine Nothing = False

deleteBranch :: String -> AppContextM ()
deleteBranch branchUuid = do
  checkPermission _KM_PERM
  branch <- findBranchById branchUuid
  deleteBranchById branchUuid
  deleteMigratorStateByBranchUuid branchUuid
  return ()

getBranchState :: BranchWithEvents -> AppContextM BranchState
getBranchState branch = isMigrating $ isEditing $ isMigrated $ isOutdated isDefault
  where
    isMigrating continue = do
      mMs <- findMigratorStateByBranchUuid' (U.toString $ branch ^. uuid)
      case mMs of
        Just ms ->
          if ms ^. migrationState == CompletedState
            then continue
            else return BSMigrating
        Nothing -> continue
    isEditing continue =
      if not (null $ branch ^. events)
        then return BSEdited
        else continue
    isMigrated continue = do
      mMs <- findMigratorStateByBranchUuid' (U.toString $ branch ^. uuid)
      case mMs of
        Just ms ->
          if ms ^. migrationState == CompletedState
            then return BSMigrated
            else continue
        Nothing -> continue
    isOutdated continue = do
      mForkOfPackageId <- getBranchForkOfPackageId branch
      case mForkOfPackageId of
        Just forkOfPackageId -> do
          newerPackages <- getNewerPackages forkOfPackageId
          if not . null $ newerPackages
            then return BSOutdated
            else continue
        Nothing -> continue
    isDefault = return BSDefault
