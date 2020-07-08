module Wizard.Api.Resource.Template.TemplateChangeJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Template.TemplateChangeDTO

instance FromJSON TemplateChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateChangeDTO where
  toJSON = genericToJSON simpleOptions
