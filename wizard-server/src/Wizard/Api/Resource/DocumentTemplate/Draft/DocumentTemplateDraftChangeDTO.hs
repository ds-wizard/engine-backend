module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO where

import GHC.Generics

import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data DocumentTemplateDraftChangeDTO = DocumentTemplateDraftChangeDTO
  { name :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)
