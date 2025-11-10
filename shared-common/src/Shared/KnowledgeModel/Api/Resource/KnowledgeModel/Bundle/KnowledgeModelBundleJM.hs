module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

instance FromJSON KnowledgeModelBundle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelBundle where
  toJSON = genericToJSON jsonOptions
