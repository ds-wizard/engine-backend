module WizardLib.Public.Model.User.RoleSimple where

import qualified Data.UUID as U
import GHC.Generics

data RoleSimple = RoleSimple
  { uuid :: U.UUID
  , name :: String
  , permissions :: [String]
  }
  deriving (Show, Eq, Generic)
