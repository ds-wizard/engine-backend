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
import Model.Branch.Branch
import Service.Branch.BranchMapper

getBranches :: Context -> IO (Either AppError [BranchDTO])
getBranches context = do
  eitherBranches <- findBranches context
  case eitherBranches of
    Right branches -> return . Right . fmap toDTO $ branches
    Left error -> return . Left $ error

createBranch :: Context -> BranchDTO -> IO (Either AppError BranchDTO)
createBranch context branchDto = do
  let artifactId = branchDto ^. bdtoArtifactId
  case isValidArtifactId artifactId of
    Nothing -> do
      eitherBranchFromDb <- findBranchByArtifactId context artifactId
      case eitherBranchFromDb of
        Right _ -> return . Left $ createErrorWithFieldError ("artifactId", "ArtifactId is already taken")
        Left (NotExistsError _) -> do
          let branch = fromDTO branchDto
          insertBranch context branch
          return . Right . toDTO $ branch
        Left error -> return . Left $ error
    Just error -> return . Left $ error

getBranchById :: Context -> String -> IO (Either AppError BranchDTO)
getBranchById context branchUuid = do
  eitherBranch <- findBranchById context branchUuid
  case eitherBranch of
    Right branch -> return . Right . toDTO $ branch
    Left error -> return . Left $ error

modifyBranch :: Context -> String -> BranchDTO -> IO (Either AppError BranchDTO)
modifyBranch context branchUuid branchDto = do
  let artifactId = branchDto ^. bdtoArtifactId
  case isValidArtifactId artifactId of
    Nothing -> do
      eitherBranch <- findBranchById context branchUuid
      case eitherBranch of
        Right branch -> do
          eitherBranchFromDb <- findBranchByArtifactId context artifactId
          if isAlreadyUsedAndIsNotMine eitherBranchFromDb
            then return . Left . createErrorWithFieldError $ ("artifactId", "ArtifactId is already taken")
            else do
              let branch = fromDTO branchDto
              updateBranchById context branch
              return . Right $ branchDto
        Left error -> return . Left $ error
    Just error -> return . Left $ error
  where
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
