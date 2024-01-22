module Wizard.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Wizard.Model.Registry.RegistryOrganization
import WizardLib.KnowledgeModel.Model.Package.Package

data PackageSimpleDTO = PackageSimpleDTO
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: PackagePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , organization :: Maybe RegistryOrganization
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
