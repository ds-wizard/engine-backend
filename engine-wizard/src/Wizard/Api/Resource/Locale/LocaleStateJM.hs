module Wizard.Api.Resource.Locale.LocaleStateJM where

import Data.Aeson

import Wizard.Model.Locale.LocaleState

instance FromJSON LocaleState

instance ToJSON LocaleState
