module Wizard.Service.Branch.BranchValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO

validateCreateDto :: BranchCreateDTO -> AppContextM ()
validateCreateDto reqDto = do
  validateCoordinatePartFormat "kmId" reqDto.kmId
  validateVersionFormat False reqDto.version
  validatePackageExistence reqDto.previousPackageId

validateChangeDto :: BranchChangeDTO -> AppContextM ()
validateChangeDto reqDto = do
  validateCoordinatePartFormat "kmId" reqDto.kmId
  validateVersionFormat False reqDto.version

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
