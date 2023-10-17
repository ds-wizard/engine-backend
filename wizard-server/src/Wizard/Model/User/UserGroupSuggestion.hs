module Wizard.Model.User.UserGroupSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data UserGroupSuggestion = UserGroupSuggestion
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , private :: Bool
  }
  deriving (Generic, Eq, Show)
