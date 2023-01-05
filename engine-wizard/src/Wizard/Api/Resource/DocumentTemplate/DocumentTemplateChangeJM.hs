module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO

instance FromJSON DocumentTemplateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateChangeDTO where
  toJSON = genericToJSON jsonOptions
