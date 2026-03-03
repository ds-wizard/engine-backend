module Wizard.Model.Project.Detail.ProjectDetailPreview where

import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion (KnowledgeModelPackageSuggestion)
import Wizard.Model.Project.Project

data ProjectDetailPreview = ProjectDetailPreview
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , documentTemplateUuid :: Maybe U.UUID
  , format :: Maybe DocumentTemplateFormatSimple
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
