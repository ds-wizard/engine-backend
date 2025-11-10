module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO

instance FromJSON DocumentTemplateDraftChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftChangeDTO where
  toJSON = genericToJSON jsonOptions
