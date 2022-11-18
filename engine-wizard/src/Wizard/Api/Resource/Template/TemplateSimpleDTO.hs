module Wizard.Api.Resource.Template.TemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Template.TemplateState

data TemplateSimpleDTO = TemplateSimpleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , remoteLatestVersion :: Maybe String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , recommendedPackageId :: Maybe String
  , formats :: [TemplateFormat]
  , usablePackages :: [PackageSimpleDTO]
  , state :: TemplateState
  , organization :: Maybe OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
