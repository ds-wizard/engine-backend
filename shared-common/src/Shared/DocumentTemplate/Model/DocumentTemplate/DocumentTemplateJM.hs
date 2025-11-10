module Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()

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
