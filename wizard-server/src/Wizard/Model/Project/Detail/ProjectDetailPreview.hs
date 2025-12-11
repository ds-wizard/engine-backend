module Wizard.Model.Project.Detail.ProjectDetailPreview where

import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Project

data ProjectDetailPreview = ProjectDetailPreview
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackageId :: String
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , documentTemplateId :: Maybe String
  , format :: Maybe DocumentTemplateFormatSimple
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
