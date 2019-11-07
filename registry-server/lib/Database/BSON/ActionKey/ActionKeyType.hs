module Database.BSON.ActionKey.ActionKeyType where

import qualified Data.Bson as BSON

import Model.ActionKey.ActionKey

instance BSON.Val ActionKeyType where
  val RegistrationActionKey = BSON.String "RegistrationActionKey"
  val ForgottenTokenActionKey = BSON.String "ForgottenTokenActionKey"
  cast' (BSON.String "RegistrationActionKey") = Just RegistrationActionKey
  cast' (BSON.String "ForgottenTokenActionKey") = Just ForgottenTokenActionKey
  cast' _ = Nothing
