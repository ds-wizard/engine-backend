module Wizard.Database.Mapping.User.UserSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.User.UserSuggestion

instance FromRow UserSuggestion where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    imageUrl <- field
    return $ UserSuggestion {..}
