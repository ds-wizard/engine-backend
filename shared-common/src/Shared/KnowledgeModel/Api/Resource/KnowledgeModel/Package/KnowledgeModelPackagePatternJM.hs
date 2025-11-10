module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

instance FromJSON KnowledgeModelPackagePattern where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackagePattern where
  toJSON = genericToJSON jsonOptions
