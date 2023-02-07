module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO

instance FromJSON DocumentTemplateDraftCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftCreateDTO where
  toJSON = genericToJSON jsonOptions
