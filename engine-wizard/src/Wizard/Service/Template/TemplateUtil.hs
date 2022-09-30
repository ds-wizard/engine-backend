module Wizard.Service.Template.TemplateUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig hiding (templateMetamodelVersion)
import Shared.Constant.Template
import Shared.Model.Template.Template
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Model.Registry.RegistryTemplate
import Wizard.Model.Template.TemplateList
import Wizard.Model.Template.TemplateState

computeTemplateState :: [RegistryTemplate] -> Template -> TemplateState
computeTemplateState tmlsFromRegistry tml =
  if not (isTemplateSupported tml)
    then UnsupportedMetamodelVersionTemplateState
    else case selectTemplateByOrgIdAndTmlId tml tmlsFromRegistry of
           Just tmlFromRegistry ->
             case compareVersion (tmlFromRegistry ^. remoteVersion) (tml ^. version) of
               LT -> UnpublishedTemplateState
               EQ -> UpToDateTemplateState
               GT -> OutdatedTemplateState
           Nothing -> UnknownTemplateState

computeTemplateState' :: TemplateList -> TemplateState
computeTemplateState' tml =
  if not (isTemplateSupported tml)
    then UnsupportedMetamodelVersionTemplateState
    else case tml ^. remoteVersion of
           Just rVersion ->
             case compareVersion rVersion (tml ^. version) of
               LT -> UnpublishedTemplateState
               EQ -> UpToDateTemplateState
               GT -> OutdatedTemplateState
           Nothing -> UnknownTemplateState

selectTemplateByOrgIdAndTmlId tml =
  L.find (\t -> (t ^. organizationId) == (tml ^. organizationId) && (t ^. templateId) == (tml ^. templateId))

selectOrganizationByOrgId tml = L.find (\org -> (org ^. organizationId) == (tml ^. organizationId))

--getUsablePackagesForTemplate :: Template -> [Package] -> [Package]
getUsablePackagesForTemplate tml = chooseTheNewest . groupPackages . filterPackages tml
  where
    filterPackages tml = filter (\pkg -> not . null $ filterTemplates (Just $ pkg ^. pId) [tml])

--    filterPackages :: TemplateList -> [Package] -> [Package]
--isTemplateSupported :: TemplateList -> Bool
isTemplateSupported tml = tml ^. metamodelVersion == templateMetamodelVersion

--filterTemplates :: Maybe String -> [TemplateList] -> [TemplateList]
filterTemplates mPkgId tmls =
  case mPkgId of
    Just pkgId -> filter (filterTemplate . splitCoordinate $ pkgId) tmls
    Nothing -> tmls
--    filterTemplate :: [String] -> TemplateList -> Bool
  where
    filterTemplate pkgIdSplit template = fitsIntoKMSpecs pkgIdSplit (template ^. allowedPackages)
