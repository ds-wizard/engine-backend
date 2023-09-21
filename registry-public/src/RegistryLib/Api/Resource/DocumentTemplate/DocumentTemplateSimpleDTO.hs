module RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , description :: String
  , organization :: Maybe OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
