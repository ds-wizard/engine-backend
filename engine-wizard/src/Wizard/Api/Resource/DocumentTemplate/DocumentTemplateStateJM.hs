module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM where

import Data.Aeson

import Wizard.Model.DocumentTemplate.DocumentTemplateState

instance FromJSON DocumentTemplateState

instance ToJSON DocumentTemplateState
