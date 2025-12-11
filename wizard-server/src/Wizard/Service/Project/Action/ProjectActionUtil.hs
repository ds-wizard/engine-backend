module Wizard.Service.Project.Action.ProjectActionUtil where

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Constant.ProjectAction
import Wizard.Model.Project.Action.ProjectAction

isProjectActionSupported :: ProjectAction -> Bool
isProjectActionSupported importer = importer.metamodelVersion == projectActionMetamodelVersion

filterProjectActions :: Maybe String -> [ProjectAction] -> [ProjectAction]
filterProjectActions mPkgId importers =
  case mPkgId of
    Just pkgId -> filter (filterProjectAction . splitCoordinate $ pkgId) importers
    Nothing -> importers
  where
    filterProjectAction :: [String] -> ProjectAction -> Bool
    filterProjectAction pkgIdSplit importer = fitsIntoKMSpecs pkgIdSplit importer.allowedPackages
