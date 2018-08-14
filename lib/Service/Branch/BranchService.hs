module Service.Branch.BranchService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe
import Data.UUID as U
import Text.Regex

import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchWithStateDTO
import Api.Resource.Organization.OrganizationDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Localization
import Model.Branch.Branch
import Model.Branch.BranchState
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.Event.Event
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Migrator.MigratorState
import Service.Branch.BranchMapper
import Service.KnowledgeModel.KnowledgeModelService
import Service.Organization.OrganizationService
import Service.Package.PackageService
import Util.Uuid

getBranches :: AppContextM (Either AppError [BranchWithStateDTO])
getBranches =
  heGetOrganization $ \organization -> do
    eitherBranches <- findBranches
    case eitherBranches of
      Right branches -> toDTOs organization branches
      Left error -> return . Left $ error
  where
    toDTOs :: OrganizationDTO -> [Branch] -> AppContextM (Either AppError [BranchWithStateDTO])
    toDTOs organization = Prelude.foldl (foldBranch organization) (return . Right $ [])
    foldBranch ::
         OrganizationDTO
      -> AppContextM (Either AppError [BranchWithStateDTO])
      -> Branch
      -> AppContextM (Either AppError [BranchWithStateDTO])
    foldBranch organization eitherDtosIO branch = do
      eitherDtos <- eitherDtosIO
      case eitherDtos of
        Right dtos -> do
          eitherBranchState <- getBranchState (U.toString $ branch ^. uuid)
          case eitherBranchState of
            Right branchState -> return . Right $ dtos ++ [toWithStateDTO branch branchState organization]
            Left error -> return . Left $ error
        Left error -> return . Left $ error

createBranch :: BranchDTO -> AppContextM (Either AppError BranchDTO)
createBranch branchDto =
  validateKmId branchDto $
  validatePackageId (branchDto ^. parentPackageId) $
  heGetOrganization $ \organization -> do
    let branch = fromDTO branchDto
    insertBranch branch
    insertEventsToBranch (U.toString $ branch ^. uuid) []
    updateKnowledgeModelByBranchId (U.toString $ branch ^. uuid) Nothing
    updateMigrationInfoIfParentPackageIdPresent branch
    createDefaultEventIfParentPackageIsNotPresent branch
    eitherKm <- recompileKnowledgeModel (U.toString $ branch ^. uuid)
    case eitherKm of
      Right km -> return . Right $ toDTO branch organization
      Left error -> return . Left $ error
  where
    validateKmId branchDto callback = do
      let bKmId = branchDto ^. kmId
      case isValidKmId bKmId of
        Nothing -> do
          eitherBranchFromDb <- findBranchByKmId bKmId
          case eitherBranchFromDb of
            Right _ -> return . Left $ createErrorWithFieldError ("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS bKmId)
            Left (NotExistsError _) -> callback
        Just error -> return . Left $ error
    validatePackageId mPackageId callback =
      case mPackageId of
        Just packageId -> do
          eitherPackage <- findPackageById packageId
          case eitherPackage of
            Right _ -> callback
            Left error ->
              return . Left $ createErrorWithFieldError ("parentPackageId", _ERROR_VALIDATION__PARENT_PKG_ABSENCE)
        Nothing -> callback
    updateMigrationInfoIfParentPackageIdPresent branch = do
      let branchUuid = U.toString $ branch ^. uuid
      let maybeParentPackageId = branch ^. parentPackageId
      case maybeParentPackageId of
        Just parentPackageId -> updateBranchWithMigrationInfo branchUuid parentPackageId parentPackageId
        Nothing -> return ()
    createDefaultEventIfParentPackageIsNotPresent branch = do
      let branchUuid = U.toString $ branch ^. uuid
      let maybeParentPackageId = branch ^. parentPackageId
      case maybeParentPackageId of
        Just _ -> return ()
        Nothing -> do
          uuid <- liftIO generateUuid
          kmUuid <- liftIO generateUuid
          let addKMEvent =
                AddKnowledgeModelEvent
                { _addKnowledgeModelEventUuid = uuid
                , _addKnowledgeModelEventKmUuid = kmUuid
                , _addKnowledgeModelEventName = "New knowledge model"
                , _addKnowledgeModelEventPath = []
                }
          insertEventsToBranch branchUuid [AddKnowledgeModelEvent' addKMEvent]

getBranchById :: String -> AppContextM (Either AppError BranchWithStateDTO)
getBranchById branchUuid =
  heGetOrganization $ \organization -> do
    eitherBranch <- findBranchById branchUuid
    case eitherBranch of
      Right branch -> do
        eitherBranchState <- getBranchState (U.toString $ branch ^. uuid)
        case eitherBranchState of
          Right branchState -> return . Right $ toWithStateDTO branch branchState organization
          Left error -> return . Left $ error
      Left error -> return . Left $ error

modifyBranch :: String -> BranchDTO -> AppContextM (Either AppError BranchDTO)
modifyBranch branchUuid branchDto =
  validateKmId $ do
    let branch = fromDTO branchDto
    updateBranchById branch
    return . Right $ branchDto
  where
    validateKmId callback = do
      let bKmId = branchDto ^. kmId
      case isValidKmId bKmId of
        Nothing -> do
          eitherBranchFromDb <- findBranchById branchUuid
          case eitherBranchFromDb of
            Right branch -> do
              eitherBranchFromDb <- findBranchByKmId bKmId
              if isAlreadyUsedAndIsNotMine eitherBranchFromDb
                then return . Left . createErrorWithFieldError $ ("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS bKmId)
                else callback
            Left error -> return . Left $ error
        Just error -> return . Left $ error
    isAlreadyUsedAndIsNotMine (Right branch) = U.toString (branch ^. uuid) /= branchUuid
    isAlreadyUsedAndIsNotMine (Left _) = False

deleteBranch :: String -> AppContextM (Maybe AppError)
deleteBranch branchUuid = do
  eitherBranch <- findBranchById branchUuid
  case eitherBranch of
    Right branch -> do
      deleteBranchById branchUuid
      deleteMigratorStateByBranchUuid branchUuid
      return Nothing
    Left error -> return . Just $ error

isValidKmId :: String -> Maybe AppError
isValidKmId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ createErrorWithFieldError ("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"

getBranchState :: String -> AppContextM (Either AppError BranchState)
getBranchState branchUuid =
  getIsMigrating $ \isMigrating ->
    if isMigrating
      then return . Right $ BSMigrating
      else getBranch $ \branch ->
             if isEditing branch
               then return . Right $ BSEdited
               else getIsMigrated $ \isMigrated ->
                      if isMigrated
                        then return . Right $ BSMigrated
                        else getIsOutdated branch $ \isOutdated ->
                               if isOutdated
                                 then return . Right $ BSOutdated
                                 else return . Right $ BSDefault
  where
    getIsMigrating callback = do
      eitherMs <- findMigratorStateByBranchUuid branchUuid
      case eitherMs of
        Right ms ->
          if ms ^. migrationState == CompletedState
            then callback False
            else callback True
        Left (NotExistsError _) -> callback False
        Left error -> return . Left $ error
    isEditing branch = Prelude.length (branch ^. events) > 0
    getIsOutdated branch callback =
      case branch ^. lastAppliedParentPackageId of
        Just lastAppliedParentPackageId -> do
          eitherNewerPackages <- getNewerPackages lastAppliedParentPackageId
          case eitherNewerPackages of
            Right newerPackages -> callback $ Prelude.length newerPackages > 0
            Left error -> return . Left $ error
        Nothing -> callback False
    getBranch callback = do
      eitherBranch <- findBranchWithEventsById branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error
    getIsMigrated callback = do
      eitherMs <- findMigratorStateByBranchUuid branchUuid
      case eitherMs of
        Right ms ->
          if ms ^. migrationState == CompletedState
            then callback True
            else callback False
        Left (NotExistsError _) -> callback False
        Left error -> return . Left $ error
