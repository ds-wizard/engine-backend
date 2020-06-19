module Wizard.Api.Resource.Template.TemplateJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Template.TemplateDTO

instance FromJSON TemplateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateDTO where
  toJSON = genericToJSON simpleOptions
