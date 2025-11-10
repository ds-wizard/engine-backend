module Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO

instance FromJSON PackagePublishEditorDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePublishEditorDTO where
  toJSON = genericToJSON jsonOptions
