module Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , description :: String
  , organization :: Maybe OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
