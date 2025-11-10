module Registry.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM where

import Data.Aeson

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawJM ()
import Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Common.Util.Aeson

instance FromJSON KnowledgeModelBundle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelBundle where
  toJSON = genericToJSON jsonOptions
