module Shared.Api.Resource.TemplateBundle.TemplateBundleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template

data TemplateBundleDTO =
  TemplateBundleDTO
    { _templateBundleDTOTId :: String
    , _templateBundleDTOName :: String
    , _templateBundleDTOOrganizationId :: String
    , _templateBundleDTOTemplateId :: String
    , _templateBundleDTOVersion :: String
    , _templateBundleDTOMetamodelVersion :: Int
    , _templateBundleDTODescription :: String
    , _templateBundleDTOReadme :: String
    , _templateBundleDTOLicense :: String
    , _templateBundleDTOAllowedPackages :: [PackagePattern]
    , _templateBundleDTORecommendedPackageId :: Maybe String
    , _templateBundleDTOFormats :: [TemplateFormat]
    , _templateBundleDTOFiles :: [TemplateFileDTO]
    , _templateBundleDTOAssets :: [TemplateAssetDTO]
    , _templateBundleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
