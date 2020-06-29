module Wizard.Service.Template.TemplateUtil where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Package.Package
import Shared.Model.Template.Template
import Wizard.Service.Package.PackageUtils
import Wizard.Util.IdentifierUtil

getAllowedPackagesForTemplate :: Template -> [Package] -> [Package]
getAllowedPackagesForTemplate tml = getNewestUniquePackages . filterPackages tml
  where
    filterPackages :: Template -> [Package] -> [Package]
    filterPackages tml = filter (\pkg -> not . null $ filterTemplates (splitPackageId $ pkg ^. pId) [tml])

filterTemplates :: [String] -> [Template] -> [Template]
filterTemplates pkgIdSplit = filter (filterTemplate pkgIdSplit)
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
