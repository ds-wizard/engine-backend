module WizardLib.Public.Model.User.RoleList where

import qualified Data.UUID as U
import GHC.Generics

data RoleList = RoleList
  { uuid :: U.UUID
  , name :: String
  , permissions :: [String]
  , usersCount :: Int
  , isAdmin :: Bool
  }
  deriving (Show, Eq, Generic)
