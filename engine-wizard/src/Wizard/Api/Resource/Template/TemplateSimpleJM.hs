module Wizard.Api.Resource.Template.TemplateSimpleJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Template.TemplateSimpleDTO

instance FromJSON TemplateSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateSimpleDTO where
  toJSON = genericToJSON simpleOptions
