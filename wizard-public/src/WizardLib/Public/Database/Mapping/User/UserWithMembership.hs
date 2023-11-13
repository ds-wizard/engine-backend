module WizardLib.Public.Database.Mapping.User.UserWithMembership where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import WizardLib.Public.Database.Mapping.User.UserGroupMembership ()
import WizardLib.Public.Model.User.UserWithMembership

instance FromRow UserWithMembership where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    imageUrl <- field
    membershipType <- field
    return $ UserWithMembership {..}
