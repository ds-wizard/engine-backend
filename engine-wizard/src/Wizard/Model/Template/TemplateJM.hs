module Wizard.Model.Template.TemplateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Template.Template

instance FromJSON Template where
  parseJSON = simpleParseJSON "_template"

instance ToJSON Template where
  toJSON = simpleToJSON "_template"

instance FromJSON TemplateAllowedPackage where
  parseJSON = simpleParseJSON "_templateAllowedPackage"

instance ToJSON TemplateAllowedPackage where
  toJSON = simpleToJSON "_templateAllowedPackage"

instance FromJSON TemplateFormat where
  parseJSON = simpleParseJSON "_templateFormat"

instance ToJSON TemplateFormat where
  toJSON = simpleToJSON "_templateFormat"
