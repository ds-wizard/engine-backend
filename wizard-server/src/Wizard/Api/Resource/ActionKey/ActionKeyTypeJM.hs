module Wizard.Api.Resource.ActionKey.ActionKeyTypeJM where

import Data.Aeson

import Wizard.Model.ActionKey.ActionKeyType

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType
