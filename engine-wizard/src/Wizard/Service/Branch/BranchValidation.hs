module Wizard.Service.Branch.BranchValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Regex

import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

validateCreateDto :: BranchCreateDTO -> AppContextM ()
validateCreateDto reqDto = do
  validateKmId reqDto.kmId
  validateVersionFormat False reqDto.version
  validatePackageExistence reqDto.previousPackageId

validateChangeDto :: BranchChangeDTO -> AppContextM ()
validateChangeDto reqDto = do
  validateKmId reqDto.kmId
  validateVersionFormat False reqDto.version

validateKmId :: String -> AppContextM ()
validateKmId kmId = forM_ (isValidKmId kmId) throwError

isValidKmId :: String -> Maybe AppError
isValidKmId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] (M.singleton "kmId" [_ERROR_VALIDATION__INVALID_KM_ID_FORMAT])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"

validatePackageExistence :: Maybe String -> AppContextM ()
validatePackageExistence mPkgId =
  case mPkgId of
    Just pkgId -> do
      mPkg <- findPackageById' pkgId
      case mPkg of
        Just _ -> return ()
        Nothing ->
          throwError $ ValidationError [] (M.singleton "previousPackageId" [_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE])
    Nothing -> return ()
