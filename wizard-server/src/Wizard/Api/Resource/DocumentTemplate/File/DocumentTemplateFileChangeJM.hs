module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO

instance FromJSON DocumentTemplateFileChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileChangeDTO where
  toJSON = genericToJSON jsonOptions
