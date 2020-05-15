module Wizard.Api.Resource.Template.TemplateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Template.TemplateJM ()

instance FromJSON TemplateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateDTO where
  toJSON = genericToJSON simpleOptions
