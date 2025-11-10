module Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret

instance FromJSON KnowledgeModelSecret where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelSecret where
  toJSON = genericToJSON jsonOptions
