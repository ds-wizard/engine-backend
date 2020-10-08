module Wizard.Model.User.UserSuggestion where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.User.User

data UserSuggestion =
  UserSuggestion
    { _userSuggestionUuid :: U.UUID
    , _userSuggestionFirstName :: String
    , _userSuggestionLastName :: String
    , _userSuggestionEmail :: Email
    , _userSuggestionImageUrl :: Maybe String
    }
  deriving (Generic, Eq, Show)
