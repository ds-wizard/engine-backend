module Service.Branch.BranchService where

import Control.Lens ((^.))
import Data.Maybe
import Data.UUID as U
import Text.Regex

import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchWithStateDTO
import Common.Context
import Common.Error
import Common.Localization
import Common.Uuid
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Model.Branch.BranchState
import Model.Event.Event
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Migrator.MigratorState
import Model.Organization.Organization
import Service.Branch.BranchMapper
import Service.KnowledgeModel.KnowledgeModelService
import Service.Package.PackageService

getBranches :: Context -> IO (Either AppError [BranchWithStateDTO])
getBranches context =
  getOrganization context $ \organization -> do
    eitherBranches <- findBranches context
    case eitherBranches of
      Right branches -> toDTOs organization branches
      Left error -> return . Left $ error
  where
    toDTOs :: Organization -> [Branch] -> IO (Either AppError [BranchWithStateDTO])
    toDTOs organization = Prelude.foldl (foldBranch organization) (return . Right $ [])
    foldBranch ::
         Organization
      -> IO (Either AppError [BranchWithStateDTO])
      -> Branch
      -> IO (Either AppError [BranchWithStateDTO])
    foldBranch organization eitherDtosIO branch = do
      eitherDtos <- eitherDtosIO
      case eitherDtos of
        Right dtos -> do
          eitherBranchState <- getBranchState context (U.toString $ branch ^. bUuid)
          case eitherBranchState of
            Right branchState -> return . Right $ dtos ++ [toWithStateDTO branch branchState organization]
            Left error -> return . Left $ error
        Left error -> return . Left $ error

createBranch :: Context -> BranchDTO -> IO (Either AppError BranchDTO)
createBranch context branchDto =
  validateArtifactId branchDto $
  validatePackageId context (branchDto ^. bdtoParentPackageId) $
  getOrganization context $ \organization -> do
    let branch = fromDTO branchDto
    insertBranch context branch
    insertEventsToBranch context (U.toString $ branch ^. bUuid) []
    updateKnowledgeModelByBranchId context (U.toString $ branch ^. bUuid) Nothing
    updateMigrationInfoIfParentPackageIdPresent branch
    createDefaultEventIfParentPackageIsNotPresent branch
    eitherKm <- recompileKnowledgeModel context (U.toString $ branch ^. bUuid)
    case eitherKm of
      Right km -> return . Right $ toDTO branch organization
      Left error -> return . Left $ error
  where
    validateArtifactId branchDto callback = do
      let artifactId = branchDto ^. bdtoArtifactId
      case isValidArtifactId artifactId of
        Nothing -> do
          eitherBranchFromDb <- findBranchByArtifactId context artifactId
          case eitherBranchFromDb of
            Right _ ->
              return . Left $
              createErrorWithFieldError ("artifactId", _ERROR_VALIDATION__ARTIFACT_ID_UNIQUENESS artifactId)
            Left (NotExistsError _) -> callback
        Just error -> return . Left $ error
    validatePackageId context mPackageId callback =
      case mPackageId of
        Just packageId -> do
          eitherPackage <- findPackageById context packageId
          case eitherPackage of
            Right _ -> callback
            Left error ->
              return . Left $ createErrorWithFieldError ("parentPackageId", _ERROR_VALIDATION__PARENT_PKG_ABSENCE)
        Nothing -> callback
    updateMigrationInfoIfParentPackageIdPresent branch = do
      let branchUuid = U.toString $ branch ^. bUuid
      let maybeParentPackageId = branch ^. bParentPackageId
      case maybeParentPackageId of
        Just parentPackageId -> updateBranchWithMigrationInfo context branchUuid parentPackageId parentPackageId
        Nothing -> return ()
    createDefaultEventIfParentPackageIsNotPresent branch = do
      let branchUuid = U.toString $ branch ^. bUuid
      let maybeParentPackageId = branch ^. bParentPackageId
      case maybeParentPackageId of
        Just _ -> return ()
        Nothing -> do
          uuid <- generateUuid
          kmUuid <- generateUuid
          let addKMEvent =
                AddKnowledgeModelEvent
                { _addKnowledgeModelEventUuid = uuid
                , _addKnowledgeModelEventKmUuid = kmUuid
                , _addKnowledgeModelEventName = "New knowledge model"
                }
          insertEventsToBranch context branchUuid [AddKnowledgeModelEvent' addKMEvent]

getBranchById :: Context -> String -> IO (Either AppError BranchWithStateDTO)
getBranchById context branchUuid =
  getOrganization context $ \organization -> do
    eitherBranch <- findBranchById context branchUuid
    case eitherBranch of
      Right branch -> do
        eitherBranchState <- getBranchState context (U.toString $ branch ^. bUuid)
        case eitherBranchState of
          Right branchState -> return . Right $ toWithStateDTO branch branchState organization
          Left error -> return . Left $ error
      Left error -> return . Left $ error

modifyBranch :: Context -> String -> BranchDTO -> IO (Either AppError BranchDTO)
modifyBranch context branchUuid branchDto =
  validateArtifactId $ do
    let branch = fromDTO branchDto
    updateBranchById context branch
    return . Right $ branchDto
  where
    validateArtifactId callback = do
      let artifactId = branchDto ^. bdtoArtifactId
      case isValidArtifactId artifactId of
        Nothing -> do
          eitherBranchFromDb <- findBranchById context branchUuid
          case eitherBranchFromDb of
            Right branch -> do
              eitherBranchFromDb <- findBranchByArtifactId context artifactId
              if isAlreadyUsedAndIsNotMine eitherBranchFromDb
                then return . Left . createErrorWithFieldError $
                     ("artifactId", _ERROR_VALIDATION__ARTIFACT_ID_UNIQUENESS artifactId)
                else callback
            Left error -> return . Left $ error
        Just error -> return . Left $ error
    isAlreadyUsedAndIsNotMine (Right branch) = U.toString (branch ^. bUuid) /= branchUuid
    isAlreadyUsedAndIsNotMine (Left _) = False

deleteBranch :: Context -> String -> IO (Maybe AppError)
deleteBranch context branchUuid = do
  eitherBranch <- findBranchById context branchUuid
  case eitherBranch of
    Right branch -> do
      deleteBranchById context branchUuid
      deleteMigratorStateByBranchUuid context branchUuid
      return Nothing
    Left error -> return . Just $ error

isValidArtifactId :: String -> Maybe AppError
isValidArtifactId artifactId =
  if isJust $ matchRegex validationRegex artifactId
    then Nothing
    else Just $ createErrorWithFieldError ("artifactId", _ERROR_VALIDATION__INVALID_ARTIFACT_FORMAT)
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"

getBranchState :: Context -> String -> IO (Either AppError BranchState)
getBranchState context branchUuid =
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
      eitherMs <- findMigratorStateByBranchUuid context branchUuid
      case eitherMs of
        Right migrationState ->
          if migrationState ^. msMigrationState == CompletedState
            then callback False
            else callback True
        Left (NotExistsError _) -> callback False
        Left error -> return . Left $ error
    isEditing branch = Prelude.length (branch ^. bweEvents) > 0
    getIsOutdated branch callback =
      case branch ^. bweLastAppliedParentPackageId of
        Just lastAppliedParentPackageId -> do
          eitherNewerPackages <- getNewerPackages context lastAppliedParentPackageId
          case eitherNewerPackages of
            Right newerPackages -> callback $ Prelude.length newerPackages > 0
            Left error -> return . Left $ error
        Nothing -> callback False
    getBranch callback = do
      eitherBranch <- findBranchWithEventsById context branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error
    getIsMigrated callback = do
      eitherMs <- findMigratorStateByBranchUuid context branchUuid
      case eitherMs of
        Right migrationState ->
          if migrationState ^. msMigrationState == CompletedState
            then callback True
            else callback False
        Left (NotExistsError _) -> callback False
        Left error -> return . Left $ error

getOrganization context callback = do
  eitherOrganization <- findOrganization context
  case eitherOrganization of
    Right organization -> callback organization
    Left error -> return . Left $ error
