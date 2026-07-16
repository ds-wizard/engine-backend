module Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList

instance ToJSON KnowledgeModelLocaleList where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelLocaleList where
  parseJSON = genericParseJSON jsonOptions
