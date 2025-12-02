module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO where

import GHC.Generics

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateDraftChangeDTO = DocumentTemplateDraftChangeDTO
  { name :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)
