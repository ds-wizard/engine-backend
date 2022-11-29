module Wizard.Api.Resource.Template.TemplateDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Template.TemplateState

data TemplateDetailDTO = TemplateDetailDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , recommendedPackageId :: Maybe String
  , formats :: [TemplateFormat]
  , usablePackages :: [PackageSimpleDTO]
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , state :: TemplateState
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
