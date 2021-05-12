module Wizard.Database.Mapping.User.UserSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.User.UserSuggestion

instance FromRow UserSuggestion where
  fromRow = do
    _userSuggestionUuid <- field
    _userSuggestionFirstName <- field
    _userSuggestionLastName <- field
    _userSuggestionEmail <- field
    _userSuggestionImageUrl <- field
    return $ UserSuggestion {..}
