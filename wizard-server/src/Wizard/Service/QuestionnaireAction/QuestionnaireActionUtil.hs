module Wizard.Service.QuestionnaireAction.QuestionnaireActionUtil where

import Wizard.Model.QuestionnaireAction.QuestionnaireAction
import WizardLib.Common.Constant.QuestionnaireAction
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

isQuestionnaireActionSupported :: QuestionnaireAction -> Bool
isQuestionnaireActionSupported importer = importer.metamodelVersion == questionnaireActionMetamodelVersion

filterQuestionnaireActions :: Maybe String -> [QuestionnaireAction] -> [QuestionnaireAction]
filterQuestionnaireActions mPkgId importers =
  case mPkgId of
    Just pkgId -> filter (filterQuestionnaireAction . splitCoordinate $ pkgId) importers
    Nothing -> importers
  where
    filterQuestionnaireAction :: [String] -> QuestionnaireAction -> Bool
    filterQuestionnaireAction pkgIdSplit importer = fitsIntoKMSpecs pkgIdSplit importer.allowedPackages
