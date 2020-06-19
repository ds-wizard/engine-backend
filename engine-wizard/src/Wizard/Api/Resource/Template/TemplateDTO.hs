module Wizard.Api.Resource.Template.TemplateDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Template.Template
import Wizard.Api.Resource.Package.PackageSimpleDTO

data TemplateDTO =
  TemplateDTO
    { _templateDTOTId :: String
    , _templateDTOName :: String
    , _templateDTOOrganizationId :: String
    , _templateDTOTemplateId :: String
    , _templateDTOVersion :: String
    , _templateDTOMetamodelVersion :: Int
    , _templateDTODescription :: String
    , _templateDTOReadme :: String
    , _templateDTOLicense :: String
    , _templateDTOAllowedPackages :: [PackageSimpleDTO]
    , _templateDTORecommendedPackageId :: Maybe String
    , _templateDTOFormats :: [TemplateFormat]
    , _templateDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
