module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO

instance FromJSON DocumentTemplateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateChangeDTO where
  toJSON = genericToJSON jsonOptions
