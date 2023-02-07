module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Package.PackagePattern
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , remoteLatestVersion :: Maybe String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  , usablePackages :: [PackageSimpleDTO]
  , state :: DocumentTemplateState
  , organization :: Maybe OrganizationSimpleDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
