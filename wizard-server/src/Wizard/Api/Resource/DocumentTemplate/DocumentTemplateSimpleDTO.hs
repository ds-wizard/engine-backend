module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

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
