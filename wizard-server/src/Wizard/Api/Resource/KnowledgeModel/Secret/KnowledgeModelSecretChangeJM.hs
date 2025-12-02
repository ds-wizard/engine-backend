module Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO

instance FromJSON KnowledgeModelSecretChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecretChangeDTO where
  toJSON = genericToJSON jsonOptions
