module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO where

import Data.Time
import GHC.Generics

import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data DocumentTemplateBundleDTO = DocumentTemplateBundleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  , files :: [DocumentTemplateFileDTO]
  , assets :: [DocumentTemplateAssetDTO]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
