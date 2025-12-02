module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO

instance FromJSON DocumentTemplateDraftCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftCreateDTO where
  toJSON = genericToJSON jsonOptions
