module RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , description :: String
  , organization :: Maybe OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
