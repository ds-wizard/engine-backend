module RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple

data KnowledgeModelPackageSimpleDTO = KnowledgeModelPackageSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , description :: String
  , organization :: OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
