module Shared.Model.Template.TemplateJM where

import Data.Aeson

import Shared.Model.Template.Template
import Shared.Util.JSON

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

instance FromJSON TemplateFormatStep where
  parseJSON = simpleParseJSON "_templateFormatStep"

instance ToJSON TemplateFormatStep where
  toJSON = simpleToJSON "_templateFormatStep"

instance FromJSON TemplateFile where
  parseJSON = simpleParseJSON "_templateFile"

instance ToJSON TemplateFile where
  toJSON = simpleToJSON "_templateFile"

instance FromJSON TemplateAsset where
  parseJSON = simpleParseJSON "_templateAsset"

instance ToJSON TemplateAsset where
  toJSON = simpleToJSON "_templateAsset"
