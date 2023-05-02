module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO where

import Data.Time
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryOrganization
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data DocumentTemplateDetailDTO = DocumentTemplateDetailDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  , usablePackages :: [PackageSimpleDTO]
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , state :: DocumentTemplateState
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
