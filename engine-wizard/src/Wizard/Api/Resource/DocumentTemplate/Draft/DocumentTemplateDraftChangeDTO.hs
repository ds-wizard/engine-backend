module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO where

import GHC.Generics

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Package.PackagePattern

data DocumentTemplateDraftChangeDTO = DocumentTemplateDraftChangeDTO
  { name :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  }
  deriving (Show, Eq, Generic)
