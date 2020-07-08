module Wizard.Api.Resource.Template.TemplateChangeDTO where

import GHC.Generics

import Shared.Model.Template.Template

data TemplateChangeDTO =
  TemplateChangeDTO
    { _templateChangeDTOName :: String
    , _templateChangeDTOOrganizationId :: String
    , _templateChangeDTOTemplateId :: String
    , _templateChangeDTOVersion :: String
    , _templateChangeDTOMetamodelVersion :: Int
    , _templateChangeDTODescription :: String
    , _templateChangeDTOReadme :: String
    , _templateChangeDTOLicense :: String
    , _templateChangeDTOAllowedPackages :: [TemplateAllowedPackage]
    , _templateChangeDTORecommendedPackageId :: Maybe String
    , _templateChangeDTOFormats :: [TemplateFormat]
    }
  deriving (Show, Eq, Generic)
