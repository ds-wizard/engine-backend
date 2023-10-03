module Wizard.Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

data UserProfileChangeDTO = UserProfileChangeDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  }
  deriving (Generic)
