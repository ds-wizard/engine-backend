module Model.User.User where

import Control.Lens (makeLenses)
import Data.UUID
import GHC.Generics

import Common.Types

data User = User
  { _uUuid :: UUID
  , _uName :: String
  , _uSurname :: String
  , _uEmail :: Email
  , _uPasswordHash :: String
  , _uRole :: Role
  , _uPermissions :: [Permission]
  } deriving (Generic, Show, Eq)

makeLenses ''User
