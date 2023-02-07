module Shared.Model.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePatternJM ()
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Aeson

instance ToJSON DocumentTemplatePhase

instance FromJSON DocumentTemplatePhase

instance FromJSON DocumentTemplate where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplate where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormat where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFormatStep where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatStep where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFile where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFile where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateAsset where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAsset where
  toJSON = genericToJSON jsonOptions
