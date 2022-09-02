module Wizard.Api.Resource.Template.TemplateDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Template.TemplateState

data TemplateDetailDTO =
  TemplateDetailDTO
    { _templateDetailDTOTId :: String
    , _templateDetailDTOName :: String
    , _templateDetailDTOOrganizationId :: String
    , _templateDetailDTOTemplateId :: String
    , _templateDetailDTOVersion :: String
    , _templateDetailDTOMetamodelVersion :: Int
    , _templateDetailDTODescription :: String
    , _templateDetailDTOReadme :: String
    , _templateDetailDTOLicense :: String
    , _templateDetailDTOAllowedPackages :: [PackagePattern]
    , _templateDetailDTORecommendedPackageId :: Maybe String
    , _templateDetailDTOFormats :: [TemplateFormat]
    , _templateDetailDTOUsablePackages :: [PackageSimpleDTO]
    , _templateDetailDTOVersions :: [String]
    , _templateDetailDTORemoteLatestVersion :: Maybe String
    , _templateDetailDTOOrganization :: Maybe RegistryOrganization
    , _templateDetailDTORegistryLink :: Maybe String
    , _templateDetailDTOState :: TemplateState
    , _templateDetailDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
