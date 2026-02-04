module Wizard.Service.KnowledgeModel.Editor.EditorValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Service.Coordinate.CoordinateValidation
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

validateCreateDto :: KnowledgeModelEditorCreateDTO -> AppContextM ()
validateCreateDto reqDto = do
  validateCoordinatePartFormat "kmId" reqDto.kmId
  validateVersionFormat False reqDto.version
  validatePackageExistence reqDto.previousPackageUuid

validateChangeDto :: KnowledgeModelEditorChangeDTO -> AppContextM ()
validateChangeDto reqDto = do
  validateCoordinatePartFormat "kmId" reqDto.kmId
  validateVersionFormat False reqDto.version

validatePackageExistence :: Maybe U.UUID -> AppContextM ()
validatePackageExistence mPkgUuid =
  case mPkgUuid of
    Just pkgUuid -> do
      mPkg <- findPackageByUuid' pkgUuid
      case mPkg of
        Just _ -> return ()
        Nothing ->
          throwError $ ValidationError [] (M.singleton "previousPackageUuid" [_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE])
    Nothing -> return ()
