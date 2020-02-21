module Wizard.Service.Branch.BranchValidation where

import Data.Maybe
import Text.Regex

import Control.Monad.Except (throwError)
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

isValidKmId :: String -> Maybe AppError
isValidKmId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] [("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)]
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"

validateNewKmId :: String -> AppContextM ()
validateNewKmId kmId =
  case isValidKmId kmId of
    Nothing -> do
      mBranch <- findBranchByKmId' kmId
      case mBranch of
        Nothing -> return ()
        Just _ -> throwError $ ValidationError [] [("kmId", _ERROR_VALIDATION__KM_ID_UNIQUENESS kmId)]
    Just error -> throwError error

validatePackageExistence :: Maybe String -> AppContextM ()
validatePackageExistence mPkgId =
  case mPkgId of
    Just pkgId -> do
      mPkg <- findPackageById' pkgId
      case mPkg of
        Just _ -> return ()
        Nothing -> throwError $ ValidationError [] [("previousPackageId", _ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE)]
    Nothing -> return ()
