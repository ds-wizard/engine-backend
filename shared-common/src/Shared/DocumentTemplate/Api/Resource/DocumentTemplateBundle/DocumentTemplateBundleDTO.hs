module Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO where

import Data.Time
import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateBundleDTO = DocumentTemplateBundleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , formats :: [DocumentTemplateFormatDTO]
  , files :: [DocumentTemplateFileDTO]
  , assets :: [DocumentTemplateAssetDTO]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
