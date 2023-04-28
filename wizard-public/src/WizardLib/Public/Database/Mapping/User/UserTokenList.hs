module WizardLib.Public.Database.Mapping.User.UserTokenList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import WizardLib.Public.Model.User.UserTokenList

instance FromRow UserTokenList where
  fromRow = do
    uuid <- field
    name <- field
    userAgent <- field
    currentSession <- field
    expiresAt <- field
    createdAt <- field
    return $ UserTokenList {..}
