module Wizard.Service.QuestionnaireAction.QuestionnaireActionUtil where

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Constant.QuestionnaireAction
import Wizard.Model.QuestionnaireAction.QuestionnaireAction

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
