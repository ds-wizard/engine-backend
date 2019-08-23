module Database.BSON.ActionKey.ActionKeyType where

import qualified Data.Bson as BSON

import Model.ActionKey.ActionKey

instance BSON.Val ActionKeyType where
  val RegistrationActionKey = BSON.String "RegistrationActionKey"
  val ForgottenPasswordActionKey = BSON.String "ForgottenPasswordActionKey"
  cast' (BSON.String "RegistrationActionKey") = Just RegistrationActionKey
  cast' (BSON.String "ForgottenPasswordActionKey") = Just ForgottenPasswordActionKey
  cast' _ = Nothing
