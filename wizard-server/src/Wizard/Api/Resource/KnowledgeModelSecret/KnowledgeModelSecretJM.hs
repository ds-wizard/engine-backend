module Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret

instance FromJSON KnowledgeModelSecret where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecret where
  toJSON = genericToJSON jsonOptions
