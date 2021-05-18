module Wizard.Service.Template.TemplateUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Constant.Template
import Shared.Model.Package.Package
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Model.Template.TemplateState

computeTemplateState :: [TemplateSimpleDTO] -> Template -> TemplateState
computeTemplateState tmlsFromRegistry tml =
  if not (isTemplateSupported tml)
    then UnsupportedMetamodelVersionTemplateState
    else case selectTemplateByOrgIdAndTmlId tml tmlsFromRegistry of
           Just tmlFromRegistry ->
             case compareVersion (tmlFromRegistry ^. version) (tml ^. version) of
               LT -> UnpublishedTemplateState
               EQ -> UpToDateTemplateState
               GT -> OutdatedTemplateState
           Nothing -> UnknownTemplateState

selectTemplateByOrgIdAndTmlId tml =
  L.find (\t -> (t ^. organizationId) == (tml ^. organizationId) && (t ^. templateId) == (tml ^. templateId))

selectOrganizationByOrgId tml = L.find (\org -> (org ^. organizationId) == (tml ^. organizationId))

getUsablePackagesForTemplate :: Template -> [Package] -> [Package]
getUsablePackagesForTemplate tml = chooseTheNewest . groupPackages . filterPackages tml
  where
    filterPackages :: Template -> [Package] -> [Package]
    filterPackages tml = filter (\pkg -> not . null $ filterTemplates (Just $ pkg ^. pId) [tml])

isTemplateSupported :: Template -> Bool
isTemplateSupported tml = tml ^. metamodelVersion == templateMetamodelVersion

filterTemplates :: Maybe String -> [Template] -> [Template]
filterTemplates mPkgId tmls =
  case mPkgId of
    Just pkgId -> filter (filterTemplate . splitCoordinate $ pkgId) tmls
    Nothing -> tmls
  where
    filterTemplate :: [String] -> Template -> Bool
    filterTemplate pkgIdSplit template = foldl (foldOverKmSpec pkgIdSplit) False (template ^. allowedPackages)
    foldOverKmSpec :: [String] -> Bool -> PackagePattern -> Bool
    foldOverKmSpec pkgIdSplit acc allowedPackages = acc || fitsIntoKMSpec pkgIdSplit allowedPackages
