module Wizard.Api.Resource.User.UserDTO where

import Data.Time
import Data.UUID
import GHC.Generics

import Wizard.Model.User.User

data UserDTO =
  UserDTO
    { _userDTOUuid :: UUID
    , _userDTOFirstName :: String
    , _userDTOLastName :: String
    , _userDTOEmail :: Email
    , _userDTOAffiliation :: Maybe String
    , _userDTOSources :: [String]
    , _userDTORole :: Role
    , _userDTOPermissions :: [Permission]
    , _userDTOActive :: Bool
    , _userDTOCreatedAt :: Maybe UTCTime
    , _userDTOUpdatedAt :: Maybe UTCTime
    }
  deriving (Show, Eq, Generic)
