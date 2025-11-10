module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

instance FromJSON DocumentTemplateFileList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileList where
  toJSON = genericToJSON jsonOptions
