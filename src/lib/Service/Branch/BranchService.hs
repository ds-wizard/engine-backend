module Service.Branch.BranchService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U
import Text.Regex

import Api.Resources.Branch.BranchDTO
import Common.Context
import Common.Error
import Common.Types
import Common.Uuid
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Service.Branch.BranchMapper
import Service.KnowledgeModel.KnowledgeModelService

getBranches :: Context -> IO (Either AppError [BranchDTO])
getBranches context = do
  eitherBranches <- findBranches context
  case eitherBranches of
    Right branches -> return . Right . fmap toDTO $ branches
    Left error -> return . Left $ error

createBranch :: Context -> BranchDTO -> IO (Either AppError BranchDTO)
createBranch context branchDto =
  validateArtifactId branchDto $
  validatePackageId context (branchDto ^. bdtoParentPackageId) $ do
    let branch = fromDTO branchDto
    insertBranch context branch
    insertEventsToBranch context (U.toString $ branch ^. bUuid) []
    eitherKm <- recompileKnowledgeModel context (U.toString $ branch ^. bUuid)
    case eitherKm of
      Right _ -> return . Right . toDTO $ branch
      Left error -> return . Left $ error
  where
    validateArtifactId branchDto callback = do
      let artifactId = branchDto ^. bdtoArtifactId
      case isValidArtifactId artifactId of
        Nothing -> do
          eitherBranchFromDb <- findBranchByArtifactId context artifactId
          case eitherBranchFromDb of
            Right _ -> return . Left $ createErrorWithFieldError ("artifactId", "ArtifactId is already taken")
            Left (NotExistsError _) -> callback
        Just error -> return . Left $ error
    validatePackageId context mPackageId callback =
      case mPackageId of
        Just packageId -> do
          eitherPackage <- findPackageById context packageId
          case eitherPackage of
            Right _ -> callback
            Left error -> return . Left $ createErrorWithFieldError ("parentPackageId", "Parent package doesn't exist")
        Nothing -> callback

getBranchById :: Context -> String -> IO (Either AppError BranchDTO)
getBranchById context branchUuid = do
  eitherBranch <- findBranchById context branchUuid
  case eitherBranch of
    Right branch -> return . Right . toDTO $ branch
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
                then return . Left . createErrorWithFieldError $ ("artifactId", "ArtifactId is already taken")
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
      return Nothing
    Left error -> return . Just $ error

isValidArtifactId :: String -> Maybe AppError
isValidArtifactId artifactId =
  if isJust $ matchRegex validationRegex artifactId
    then Nothing
    else Just $ createErrorWithFieldError ("artifactId", "ArtifactId is not in valid format")
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"
