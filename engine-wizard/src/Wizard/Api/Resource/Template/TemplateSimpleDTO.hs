module Wizard.Api.Resource.Template.TemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Template.Template

data TemplateSimpleDTO =
  TemplateSimpleDTO
    { _templateSimpleDTOTId :: String
    , _templateSimpleDTOName :: String
    , _templateSimpleDTOOrganizationId :: String
    , _templateSimpleDTOTemplateId :: String
    , _templateSimpleDTOVersion :: String
    , _templateSimpleDTOMetamodelVersion :: Int
    , _templateSimpleDTODescription :: String
    , _templateSimpleDTOReadme :: String
    , _templateSimpleDTOLicense :: String
    , _templateSimpleDTOAllowedPackages :: [TemplateAllowedPackage]
    , _templateSimpleDTORecommendedPackageId :: Maybe String
    , _templateSimpleDTOFormats :: [TemplateFormat]
    , _templateSimpleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
