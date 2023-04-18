module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterUtil where

import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import WizardLib.Common.Constant.QuestionnaireImporter
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

isQuestionnaireImporterSupported :: QuestionnaireImporter -> Bool
isQuestionnaireImporterSupported importer = importer.metamodelVersion == questionnaireImporterMetamodelVersion

filterQuestionnaireImporters :: Maybe String -> [QuestionnaireImporter] -> [QuestionnaireImporter]
filterQuestionnaireImporters mPkgId importers =
  case mPkgId of
    Just pkgId -> filter (filterQuestionnaireImporter . splitCoordinate $ pkgId) importers
    Nothing -> importers
  where
    filterQuestionnaireImporter :: [String] -> QuestionnaireImporter -> Bool
    filterQuestionnaireImporter pkgIdSplit importer = fitsIntoKMSpecs pkgIdSplit importer.allowedPackages
