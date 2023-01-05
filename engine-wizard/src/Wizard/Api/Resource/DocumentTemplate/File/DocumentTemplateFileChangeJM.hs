module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO

instance FromJSON DocumentTemplateFileChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileChangeDTO where
  toJSON = genericToJSON jsonOptions
