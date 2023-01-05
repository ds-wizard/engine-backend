module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList

instance FromJSON DocumentTemplateDraftList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftList where
  toJSON = genericToJSON jsonOptions
