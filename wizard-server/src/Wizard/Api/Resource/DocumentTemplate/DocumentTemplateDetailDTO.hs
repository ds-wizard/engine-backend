module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO where

import Data.Time
import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryOrganization

data DocumentTemplateDetailDTO = DocumentTemplateDetailDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormat]
  , nonEditable :: Bool
  , usableKnowledgeModels :: [KnowledgeModelPackageSimpleDTO]
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , organization :: Maybe RegistryOrganization
  , registryLink :: Maybe String
  , state :: DocumentTemplateState
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
