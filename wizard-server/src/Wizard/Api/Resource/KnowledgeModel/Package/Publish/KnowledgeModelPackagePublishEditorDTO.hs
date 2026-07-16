module Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO where

import qualified Data.UUID as U
import GHC.Generics

data PackagePublishEditorDTO = PackagePublishEditorDTO
  { editorUuid :: U.UUID
  , localeUuids :: Maybe [U.UUID]
  }
  deriving (Generic)
