module Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion where

import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data DocumentTemplateSuggestion = DocumentTemplateSuggestion
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  }
  deriving (Show, Eq, Generic)
