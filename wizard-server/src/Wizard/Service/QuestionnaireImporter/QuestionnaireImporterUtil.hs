module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterUtil where

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Constant.QuestionnaireImporter
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

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
