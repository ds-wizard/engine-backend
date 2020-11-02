module Wizard.Api.Resource.User.UserChangeDTO where

import GHC.Generics

data UserChangeDTO =
  UserChangeDTO
    { _userChangeDTOFirstName :: String
    , _userChangeDTOLastName :: String
    , _userChangeDTOEmail :: String
    , _userChangeDTOAffiliation :: Maybe String
    , _userChangeDTORole :: String
    , _userChangeDTOActive :: Bool
    }
  deriving (Generic)
