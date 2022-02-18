module Wizard.Service.Branch.BranchService where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
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
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.BranchState
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Service.Acl.AclService
import Wizard.Service.Branch.BranchMapper
import Wizard.Service.Branch.BranchUtil
import Wizard.Service.Branch.BranchValidation
import Wizard.Service.Branch.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Limit.AppLimitService

getBranchesPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchDTO)
getBranchesPage mQuery pageable sort =
  runInTransaction $ do
    checkPermission _KM_PERM
    bs <- findBranchesPage mQuery pageable sort
    traverse enhanceBranch bs

createBranch :: BranchCreateDTO -> AppContextM BranchDTO
createBranch reqDto =
  runInTransaction $ do
    bUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    currentUser <- getCurrentUser
    createBranchWithParams bUuid now currentUser reqDto

createBranchWithParams :: U.UUID -> UTCTime -> UserDTO -> BranchCreateDTO -> AppContextM BranchDTO
createBranchWithParams bUuid now currentUser reqDto =
  runInTransaction $ do
    checkBranchLimit
    checkPermission _KM_PERM
    validateNewKmId (reqDto ^. kmId)
    validatePackageExistence (reqDto ^. previousPackageId)
    appUuid <- asks _appContextAppUuid
    let branch = fromCreateDTO reqDto bUuid (Just $ currentUser ^. uuid) appUuid now now
    insertBranch branch
    insertBranchData (toBranchData branch)
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
                  , _addKnowledgeModelEventAnnotations = []
                  , _addKnowledgeModelEventCreatedAt = now
                  }
          appendBranchEventByUuid branchUuid [AddKnowledgeModelEvent' addKMEvent]
          return ()

getBranchById :: String -> AppContextM BranchDetailDTO
getBranchById branchUuid =
  runInTransaction $ do
    checkPermission _KM_PERM
    branch <- findBranchById branchUuid
    branchData <- findBranchDataById branchUuid
    mForkOfPackageId <- getBranchForkOfPackageId branch
    branchState <- getBranchState branch branchData
    knowledgeModel <- compileKnowledgeModel [] (branch ^. previousPackageId) []
    mForkOfPackage <-
      case mForkOfPackageId of
        Just pkgId -> do
          pkg <- findPackageById pkgId
          return . Just $ pkg
        Nothing -> return Nothing
    return $ toDetailDTO branch branchData knowledgeModel mForkOfPackageId mForkOfPackage branchState

modifyBranch :: String -> BranchChangeDTO -> AppContextM BranchDetailDTO
modifyBranch branchUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    branchFromDB <- findBranchById branchUuid
    validateKmId
    now <- liftIO getCurrentTime
    let branch =
          fromChangeDTO
            reqDto
            (branchFromDB ^. uuid)
            (branchFromDB ^. previousPackageId)
            (branchFromDB ^. ownerUuid)
            (branchFromDB ^. appUuid)
            (branchFromDB ^. createdAt)
            now
    updateBranchById branch
    mForkOfPackageId <- getBranchForkOfPackageId branch
    branchData <- findBranchDataById (U.toString $ branch ^. uuid)
    branchState <- getBranchState branch branchData
    knowledgeModel <- compileKnowledgeModel (branchData ^. events) (branch ^. previousPackageId) []
    mForkOfPackage <-
      case mForkOfPackageId of
        Just pkgId -> do
          pkg <- findPackageById pkgId
          return . Just $ pkg
        Nothing -> return Nothing
    return $ toDetailDTO branch branchData knowledgeModel mForkOfPackageId mForkOfPackage branchState
  where
    validateKmId = do
      let bKmId = reqDto ^. kmId
      case isValidKmId bKmId of
        Nothing -> do
          branch <- findBranchById branchUuid
          mBranchFromDb <- findBranchByKmId' bKmId
          when
            (isAlreadyUsedAndIsNotMine mBranchFromDb)
            (throwError $ ValidationError [] (M.singleton "kmId" [_ERROR_VALIDATION__KM_ID_UNIQUENESS bKmId]))
        Just error -> throwError error
    isAlreadyUsedAndIsNotMine (Just branch) = U.toString (branch ^. uuid) /= branchUuid
    isAlreadyUsedAndIsNotMine Nothing = False

deleteBranch :: String -> AppContextM ()
deleteBranch branchUuid =
  runInTransaction $ do
    checkPermission _KM_PERM
    branch <- findBranchById branchUuid
    deleteBranchDataById branchUuid
    deleteBranchById branchUuid
    deleteMigratorStateByBranchUuid branchUuid
    logOutOnlineUsersWhenBranchDramaticallyChanged branchUuid
    return ()
