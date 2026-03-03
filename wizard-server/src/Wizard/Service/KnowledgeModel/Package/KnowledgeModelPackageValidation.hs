module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

validateIsVersionHigher :: String -> String -> AppContextM ()
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then return ()
    else throwError . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

validatePackageIdUniqueness :: Coordinate -> AppContextM ()
validatePackageIdUniqueness pkgCoordinate = do
  mPkg <- findPackageByCoordinate' pkgCoordinate
  case mPkg of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS (show pkgCoordinate)

validatePreviousPackageIdExistence :: Coordinate -> Coordinate -> AppContextM ()
validatePreviousPackageIdExistence pkgCoordinate previousPkgCoordinate = do
  mPkg <- findPackageByCoordinate' previousPkgCoordinate
  case mPkg of
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST (show previousPkgCoordinate) (show pkgCoordinate)

validateMaybePreviousPackageIdExistence :: Coordinate -> Maybe Coordinate -> AppContextM ()
validateMaybePreviousPackageIdExistence pkgCoordinate mPreviousPkgCoordinate =
  forM_ mPreviousPkgCoordinate (validatePreviousPackageIdExistence pkgCoordinate)
