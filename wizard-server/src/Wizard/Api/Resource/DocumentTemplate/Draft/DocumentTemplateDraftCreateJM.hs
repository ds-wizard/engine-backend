module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateDraftCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftCreateDTO where
  toJSON = genericToJSON jsonOptions
