module Shared.Model.Template.TemplateJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePatternJM ()
import Shared.Model.Template.Template
import Shared.Util.Aeson

instance FromJSON Template where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Template where
  toJSON = genericToJSON jsonOptions

instance FromJSON TemplateFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateFormat where
  toJSON = genericToJSON jsonOptions

instance FromJSON TemplateFormatStep where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateFormatStep where
  toJSON = genericToJSON jsonOptions

instance FromJSON TemplateFile where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateFile where
  toJSON = genericToJSON jsonOptions

instance FromJSON TemplateAsset where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateAsset where
  toJSON = genericToJSON jsonOptions
