module Registry.Api.Resource.Template.TemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

data TemplateSimpleDTO =
  TemplateSimpleDTO
    { _templateSimpleDTOTId :: String
    , _templateSimpleDTOName :: String
    , _templateSimpleDTOOrganizationId :: String
    , _templateSimpleDTOTemplateId :: String
    , _templateSimpleDTOVersion :: String
    , _templateSimpleDTODescription :: String
    , _templateSimpleDTOOrganization :: Maybe OrganizationSimpleDTO
    , _templateSimpleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
