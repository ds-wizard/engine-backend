module Wizard.Api.Resource.Template.File.TemplateFileChangeJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO

instance FromJSON TemplateFileChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateFileChangeDTO where
  toJSON = genericToJSON simpleOptions
