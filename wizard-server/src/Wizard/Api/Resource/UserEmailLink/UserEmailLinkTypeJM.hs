module Wizard.Api.Resource.UserEmailLink.UserEmailLinkTypeJM where

import Data.Aeson

import Wizard.Model.UserEmailLink.UserEmailLinkType

instance FromJSON UserEmailLinkType

instance ToJSON UserEmailLinkType
