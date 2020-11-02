module Wizard.Service.Template.TemplateUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Constant.Template
import Shared.Model.Package.Package
import Shared.Model.Template.Template
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Model.Template.TemplateState

computeTemplateState :: [TemplateSimpleDTO] -> Template -> TemplateState
computeTemplateState tmlsFromRegistry tml =
  if tml ^. metamodelVersion /= templateMetamodelVersion
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

filterTemplates :: Maybe String -> [Template] -> [Template]
filterTemplates mPkgId tmls =
  case mPkgId of
    Just pkgId -> filter (filterTemplate . splitCoordinate $ pkgId) tmls
    Nothing -> tmls
  where
    filterTemplate :: [String] -> Template -> Bool
    filterTemplate pkgIdSplit template = foldl (foldOverKmSpec pkgIdSplit) False (template ^. allowedPackages)
    foldOverKmSpec :: [String] -> Bool -> TemplateAllowedPackage -> Bool
    foldOverKmSpec pkgIdSplit acc allowedPackages = acc || fitsIntoKMSpec pkgIdSplit allowedPackages

fitsIntoKMSpec ::
     ( HasOrgId kmSpec (Maybe String)
     , HasKmId kmSpec (Maybe String)
     , HasMinVersion kmSpec (Maybe String)
     , HasMaxVersion kmSpec (Maybe String)
     )
  => [String]
  -> kmSpec
  -> Bool
fitsIntoKMSpec pkgIdSplit kmSpec = heCompareOrgId $ heCompareKmId $ heCompareVersionMin $ heCompareVersionMax True
  where
    heCompareOrgId callback =
      case kmSpec ^. orgId of
        Just orgId -> (head pkgIdSplit == orgId) && callback
        Nothing -> callback
    heCompareKmId callback =
      case kmSpec ^. kmId of
        Just kmId -> ((pkgIdSplit !! 1) == kmId) && callback
        Nothing -> callback
    heCompareVersionMin callback =
      case kmSpec ^. minVersion of
        Just minVersion ->
          case compareVersion (pkgIdSplit !! 2) minVersion of
            LT -> False
            _ -> callback
        Nothing -> callback
    heCompareVersionMax callback =
      case kmSpec ^. maxVersion of
        Just maxVersion ->
          case compareVersion (pkgIdSplit !! 2) maxVersion of
            GT -> False
            _ -> callback
        Nothing -> callback
