module Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors

instance ToSchema PackagePublishEditorDTO where
  declareNamedSchema = toSwagger packagePublishEditorDTO
