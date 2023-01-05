module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO

instance FromJSON DocumentTemplateDraftChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftChangeDTO where
  toJSON = genericToJSON jsonOptions
