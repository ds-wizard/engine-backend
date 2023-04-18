module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateFileChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileChangeDTO where
  toJSON = genericToJSON jsonOptions
