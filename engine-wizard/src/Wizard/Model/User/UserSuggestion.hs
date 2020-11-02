module Wizard.Model.User.UserSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data UserSuggestion =
  UserSuggestion
    { _userSuggestionUuid :: U.UUID
    , _userSuggestionFirstName :: String
    , _userSuggestionLastName :: String
    , _userSuggestionEmail :: String
    , _userSuggestionImageUrl :: Maybe String
    }
  deriving (Generic, Eq, Show)
