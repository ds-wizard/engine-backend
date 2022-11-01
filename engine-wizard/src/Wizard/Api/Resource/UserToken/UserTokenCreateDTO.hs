module Wizard.Api.Resource.UserToken.UserTokenCreateDTO where

import GHC.Generics

data UserTokenCreateDTO =
  UserTokenCreateDTO
    { _userTokenCreateDTOEmail :: String
    , _userTokenCreateDTOPassword :: String
    }
  deriving (Generic)
