module Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion where

import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateSuggestion = DocumentTemplateSuggestion
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormatSimple]
  }
  deriving (Show, Eq, Generic)
