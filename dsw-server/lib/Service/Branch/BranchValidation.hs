module Service.Branch.BranchValidation where

import Data.Maybe
import Text.Regex

import Database.DAO.Branch.BranchDAO
import Database.DAO.Package.PackageDAO
import Localization.Messages.Public
import Model.Context.AppContext
import Model.Error.Error
import Util.Helper (createHmeHelper)

isValidKmId :: String -> Maybe AppError
isValidKmId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] [("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)]
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"

validateNewKmId :: String -> AppContextM (Maybe AppError)
validateNewKmId kmId = do
  case isValidKmId kmId of
    Nothing -> do
      eResult <- findBranchByKmId kmId
      case eResult of
        Left (NotExistsError _) -> return Nothing
        Right _ -> return . Just $ ValidationError [] [("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS kmId)]
        Left error -> return . Just $ error
    Just error -> return . Just $ error

validatePackageExistence :: Maybe String -> AppContextM (Maybe AppError)
validatePackageExistence mPkgId =
  case mPkgId of
    Just pkgId -> do
      ePkg <- findPackageById pkgId
      case ePkg of
        Right _ -> return Nothing
        Left error ->
          return . Just $ ValidationError [] [("previousPackageId", _ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE)]
    Nothing -> return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateNewKmId kmId = createHmeHelper (validateNewKmId kmId)

-- -----------------------------------------------------
heValidatePackageExistence mPkgId = createHmeHelper (validatePackageExistence mPkgId)
