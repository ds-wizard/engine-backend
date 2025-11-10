module Wizard.Service.KnowledgeModel.Editor.EditorValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M

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
  validatePackageExistence reqDto.previousPackageId

validateChangeDto :: KnowledgeModelEditorChangeDTO -> AppContextM ()
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
