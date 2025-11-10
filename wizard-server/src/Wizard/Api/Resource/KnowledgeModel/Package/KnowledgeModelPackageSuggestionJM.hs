module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

instance FromJSON KnowledgeModelPackageSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageSuggestion where
  toJSON = genericToJSON jsonOptions
