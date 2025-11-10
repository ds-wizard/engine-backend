module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawJM where

import Data.Aeson

import Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()

instance ToJSON KnowledgeModelPackageRaw where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageRaw where
  parseJSON = genericParseJSON jsonOptions
