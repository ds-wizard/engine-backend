module Registry.Api.Resource.Template.TemplateDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

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
    , _templateDetailDTOVersions :: [String]
    , _templateDetailDTOOrganization :: OrganizationSimpleDTO
    , _templateDetailDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
