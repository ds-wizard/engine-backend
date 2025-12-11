module Wizard.Service.Project.Importer.ProjectImporterUtil where

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Constant.ProjectImporter
import Wizard.Model.Project.Importer.ProjectImporter

isProjectImporterSupported :: ProjectImporter -> Bool
isProjectImporterSupported importer = importer.metamodelVersion == projectImporterMetamodelVersion

filterProjectImporters :: Maybe String -> [ProjectImporter] -> [ProjectImporter]
filterProjectImporters mPkgId importers =
  case mPkgId of
    Just pkgId -> filter (filterProjectImporter . splitCoordinate $ pkgId) importers
    Nothing -> importers
  where
    filterProjectImporter :: [String] -> ProjectImporter -> Bool
    filterProjectImporter pkgIdSplit importer = fitsIntoKMSpecs pkgIdSplit importer.allowedPackages
