module Wizard.Service.Branch.BranchService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Package.PackageDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Event.Event
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchList
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

getBranchesPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchList)
getBranchesPage mQuery pageable sort = do
  checkPermission _KM_PERM
  findBranchesPage mQuery pageable sort

createBranch :: BranchCreateDTO -> AppContextM BranchList
createBranch reqDto =
  runInTransaction $ do
    bUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    currentUser <- getCurrentUser
    createBranchWithParams bUuid now currentUser reqDto

createBranchWithParams :: U.UUID -> UTCTime -> UserDTO -> BranchCreateDTO -> AppContextM BranchList
createBranchWithParams uuid now currentUser reqDto =
  runInTransaction $ do
    checkBranchLimit
    checkPermission _KM_PERM
    validateCreateDto reqDto
    appUuid <- asks currentAppUuid
    mPreviousPkg <-
      case reqDto.previousPackageId of
        Just previousPackageId -> fmap Just $ findPackageById previousPackageId
        Nothing -> return Nothing
    let branch = fromCreateDTO reqDto uuid mPreviousPkg currentUser.uuid appUuid now
    insertBranch branch
    insertBranchData (toBranchData branch)
    createDefaultEventIfPreviousPackageIsNotPresent branch
    return $ toList branch Nothing BSDefault
  where
    createDefaultEventIfPreviousPackageIsNotPresent branch = do
      let mPreviousPackageId = branch.previousPackageId
      case mPreviousPackageId of
        Just _ -> return ()
        Nothing -> do
          uuid <- liftIO generateUuid
          kmUuid <- liftIO generateUuid
          let addKMEvent =
                AddKnowledgeModelEvent
                  { uuid = uuid
                  , parentUuid = U.nil
                  , entityUuid = kmUuid
                  , annotations = []
                  , createdAt = now
                  }
          appendBranchEventByUuid branch.uuid [AddKnowledgeModelEvent' addKMEvent]
          return ()

getBranchById :: U.UUID -> AppContextM BranchDetailDTO
getBranchById branchUuid = do
  checkPermission _KM_PERM
  branch <- findBranchByUuid branchUuid
  branchData <- findBranchDataById branchUuid
  mForkOfPackageId <- getBranchForkOfPackageId branch
  branchState <- getBranchState branch (length branchData.events) mForkOfPackageId
  knowledgeModel <- compileKnowledgeModel [] branch.previousPackageId []
  mForkOfPackage <-
    case mForkOfPackageId of
      Just pkgId -> do
        pkg <- findPackageById pkgId
        return . Just $ pkg
      Nothing -> return Nothing
  return $ toDetailDTO branch branchData knowledgeModel mForkOfPackageId mForkOfPackage branchState

modifyBranch :: U.UUID -> BranchChangeDTO -> AppContextM BranchDetailDTO
modifyBranch branchUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    branchFromDB <- findBranchByUuid branchUuid
    validateChangeDto reqDto
    now <- liftIO getCurrentTime
    let branch =
          fromChangeDTO
            reqDto
            branchFromDB
            now
    updateBranchById branch
    mForkOfPackageId <- getBranchForkOfPackageId branch
    branchData <- findBranchDataById branch.uuid
    branchState <- getBranchState branch (length branchData.events) mForkOfPackageId
    knowledgeModel <- compileKnowledgeModel branchData.events branch.previousPackageId []
    mForkOfPackage <-
      case mForkOfPackageId of
        Just pkgId -> do
          pkg <- findPackageById pkgId
          return . Just $ pkg
        Nothing -> return Nothing
    return $ toDetailDTO branch branchData knowledgeModel mForkOfPackageId mForkOfPackage branchState

deleteBranch :: U.UUID -> AppContextM ()
deleteBranch branchUuid =
  runInTransaction $ do
    checkPermission _KM_PERM
    branch <- findBranchByUuid branchUuid
    deleteBranchDataById branchUuid
    deleteBranchByUuid branchUuid
    deleteMigratorStateByBranchUuid branchUuid
    logOutOnlineUsersWhenBranchDramaticallyChanged branchUuid
    return ()
