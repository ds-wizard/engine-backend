module Registry.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Registry.Model.ActionKey.ActionKeyType

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType
