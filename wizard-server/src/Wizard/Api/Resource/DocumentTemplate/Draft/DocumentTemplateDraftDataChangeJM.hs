module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO

instance FromJSON DocumentTemplateDraftDataChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDataChangeDTO where
  toJSON = genericToJSON jsonOptions
