module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateChangeDTO where
  toJSON = genericToJSON jsonOptions
