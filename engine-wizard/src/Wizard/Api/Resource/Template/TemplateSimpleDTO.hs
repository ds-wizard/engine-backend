module Wizard.Api.Resource.Template.TemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Template.TemplateState

data TemplateSimpleDTO =
  TemplateSimpleDTO
    { _templateSimpleDTOTId :: String
    , _templateSimpleDTOName :: String
    , _templateSimpleDTOOrganizationId :: String
    , _templateSimpleDTOTemplateId :: String
    , _templateSimpleDTOVersion :: String
    , _templateSimpleDTORemoteLatestVersion :: Maybe String
    , _templateSimpleDTOMetamodelVersion :: Int
    , _templateSimpleDTODescription :: String
    , _templateSimpleDTOReadme :: String
    , _templateSimpleDTOLicense :: String
    , _templateSimpleDTOAllowedPackages :: [PackagePattern]
    , _templateSimpleDTORecommendedPackageId :: Maybe String
    , _templateSimpleDTOFormats :: [TemplateFormat]
    , _templateSimpleDTOUsablePackages :: [PackageSimpleDTO]
    , _templateSimpleDTOState :: TemplateState
    , _templateSimpleDTOOrganization :: Maybe OrganizationSimpleDTO
    , _templateSimpleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
