module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterUtil where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.QuestionnaireImporter
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

isQuestionnaireImporterSupported :: QuestionnaireImporter -> Bool
isQuestionnaireImporterSupported importer = importer ^. metamodelVersion == questionnaireImporterMetamodelVersion

filterQuestionnaireImporters :: Maybe String -> [QuestionnaireImporter] -> [QuestionnaireImporter]
filterQuestionnaireImporters mPkgId importers =
  case mPkgId of
    Just pkgId -> filter (filterQuestionnaireImporter . splitCoordinate $ pkgId) importers
    Nothing -> importers
  where
    filterQuestionnaireImporter :: [String] -> QuestionnaireImporter -> Bool
    filterQuestionnaireImporter pkgIdSplit importer = fitsIntoKMSpecs pkgIdSplit (importer ^. allowedPackages)
