module Wizard.Api.Resource.Template.TemplateStateJM where

import Data.Aeson

import Wizard.Model.Template.TemplateState

instance FromJSON TemplateState

instance ToJSON TemplateState
