module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateDraftChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftChangeDTO where
  toJSON = genericToJSON jsonOptions
