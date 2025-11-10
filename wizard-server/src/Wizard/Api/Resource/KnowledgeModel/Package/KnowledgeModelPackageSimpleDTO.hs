module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Registry.RegistryOrganization

data KnowledgeModelPackageSimpleDTO = KnowledgeModelPackageSimpleDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , organization :: Maybe RegistryOrganization
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
