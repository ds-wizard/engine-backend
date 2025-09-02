module Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeDTO

instance FromJSON KnowledgeModelSecretChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecretChangeDTO where
  toJSON = genericToJSON jsonOptions
