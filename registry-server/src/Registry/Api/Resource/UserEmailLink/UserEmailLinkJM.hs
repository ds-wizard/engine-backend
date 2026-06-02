module Registry.Api.Resource.UserEmailLink.UserEmailLinkJM where

import Data.Aeson

import Registry.Model.UserEmailLink.UserEmailLinkType

instance FromJSON UserEmailLinkType

instance ToJSON UserEmailLinkType
