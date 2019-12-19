module Wizard.Service.Branch.BranchService
  ( getBranches
  , createBranch
  , createBranchWithParams
  , getBranchById
  , modifyBranch
  , deleteBranch
  , getBranchState
  -- Helpers
  , heGetBranchById
  , hmGetBranchById
  , heGetBranchState
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import Data.UUID as U

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.LensesConfig
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchMapper
import Wizard.Service.Branch.BranchUtils
import Wizard.Service.Branch.BranchValidation
import Wizard.Service.Migration.KnowledgeModel.MigratorService
import Wizard.Service.Organization.OrganizationService
import Wizard.Service.Package.PackageService

getBranches :: AppContextM (Either AppError [BranchDTO])
getBranches = heGetOrganization $ \organization -> heFindBranchesWithEvents $ \bs -> toDTOs organization bs
  where
    toDTOs :: OrganizationDTO -> [BranchWithEvents] -> AppContextM (Either AppError [BranchDTO])
    toDTOs organization = Prelude.foldl (foldBranch organization) (return . Right $ [])
    foldBranch ::
         OrganizationDTO
      -> AppContextM (Either AppError [BranchDTO])
      -> BranchWithEvents
      -> AppContextM (Either AppError [BranchDTO])
    foldBranch organization eitherDtosIO branch = do
      eitherDtos <- eitherDtosIO
      case eitherDtos of
        Right dtos ->
          heGetBranchForkOfPackageId branch $ \mForkOfPackageId ->
            heGetBranchState branch $ \branchState ->
              return . Right $ dtos ++ [toDTO branch mForkOfPackageId branchState organization]
        Left error -> return . Left $ error

createBranch :: BranchCreateDTO -> AppContextM (Either AppError BranchDTO)
createBranch reqDto = do
  bUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  heGetCurrentUser $ \currentUser -> createBranchWithParams bUuid now currentUser reqDto

createBranchWithParams :: U.UUID -> UTCTime -> UserDTO -> BranchCreateDTO -> AppContextM (Either AppError BranchDTO)
createBranchWithParams bUuid now currentUser reqDto =
  heValidateNewKmId (reqDto ^. kmId) $
  heValidatePackageExistence (reqDto ^. previousPackageId) $
  heGetOrganization $ \organization -> do
    let branch = fromCreateDTO reqDto bUuid (Just $ currentUser ^. uuid) now now
    insertBranch branch
    createDefaultEventIfPreviousPackageIsNotPresent branch
    return . Right $ toDTO branch Nothing BSDefault organization
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

getBranchById :: String -> AppContextM (Either AppError BranchDetailDTO)
getBranchById branchUuid =
  heGetOrganization $ \organization ->
    heFindBranchWithEventsById branchUuid $ \branch ->
      heGetBranchForkOfPackageId branch $ \mForkOfPackageId ->
        heGetBranchState branch $ \branchState ->
          return . Right $ toDetailDTO branch mForkOfPackageId branchState organization

modifyBranch :: String -> BranchChangeDTO -> AppContextM (Either AppError BranchDetailDTO)
modifyBranch branchUuid reqDto =
  heGetOrganization $ \organization ->
    heFindBranchById branchUuid $ \branchFromDB ->
      validateKmId $ do
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
        heGetBranchForkOfPackageId branch $ \mForkOfPackageId ->
          heGetBranchState branch $ \branchState ->
            return . Right $ toDetailDTO branch mForkOfPackageId branchState organization
  where
    validateKmId callback = do
      let bKmId = reqDto ^. kmId
      case isValidKmId bKmId of
        Nothing -> do
          heFindBranchById branchUuid $ \branch -> do
            eitherBranchFromDb <- findBranchByKmId bKmId
            if isAlreadyUsedAndIsNotMine eitherBranchFromDb
              then return . Left $ ValidationError [] [("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS bKmId)]
              else callback
        Just error -> return . Left $ error
    isAlreadyUsedAndIsNotMine (Right branch) = U.toString (branch ^. uuid) /= branchUuid
    isAlreadyUsedAndIsNotMine (Left _) = False

deleteBranch :: String -> AppContextM (Maybe AppError)
deleteBranch branchUuid =
  hmFindBranchById branchUuid $ \branch -> do
    deleteBranchById branchUuid
    deleteMigratorStateByBranchUuid branchUuid
    return Nothing

getBranchState :: BranchWithEvents -> AppContextM (Either AppError BranchState)
getBranchState branch = heIsMigrating $ heIsEditing $ heIsMigrated $ heIsOutdated $ heIsDefault
  where
    heIsMigrating callback = do
      eitherMs <- getCurrentMigration (U.toString $ branch ^. uuid)
      case eitherMs of
        Right ms ->
          if ms ^. migrationState == CompletedState
            then callback
            else return . Right $ BSMigrating
        Left (NotExistsError _) -> callback
        Left error -> return . Left $ error
    heIsEditing callback =
      if Prelude.length (branch ^. events) > 0
        then return . Right $ BSEdited
        else callback
    heIsMigrated callback = do
      eitherMs <- getCurrentMigration (U.toString $ branch ^. uuid)
      case eitherMs of
        Right ms ->
          if ms ^. migrationState == CompletedState
            then return . Right $ BSMigrated
            else callback
        Left (NotExistsError _) -> callback
        Left error -> return . Left $ error
    heIsOutdated callback =
      heGetBranchForkOfPackageId branch $ \mForkOfPackageId ->
        case mForkOfPackageId of
          Just forkOfPackageId ->
            heGetNewerPackages forkOfPackageId $ \newerPackages ->
              if Prelude.length newerPackages > 0
                then return . Right $ BSOutdated
                else callback
          Nothing -> callback
    heIsDefault = return . Right $ BSDefault

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetBranchById branchUuid callback = do
  eitherBranch <- getBranchById branchUuid
  case eitherBranch of
    Right branch -> callback branch
    Left error -> return . Left $ error

hmGetBranchById branchUuid callback = do
  eitherBranch <- getBranchById branchUuid
  case eitherBranch of
    Right branch -> callback branch
    Left error -> return . Just $ error

-- -----------------------------------------------------
heGetBranchState branch callback = do
  eitherBranchState <- getBranchState branch
  case eitherBranchState of
    Right branchState -> callback branchState
    Left error -> return . Left $ error
