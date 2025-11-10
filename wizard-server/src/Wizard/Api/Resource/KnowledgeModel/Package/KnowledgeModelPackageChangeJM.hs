module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO

instance FromJSON KnowledgeModelPackageChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageChangeDTO where
  toJSON = genericToJSON jsonOptions
