module Registry.Model.ActionKey.ActionKeyType where

import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenTokenActionKey
  deriving (Show, Eq, Generic, Read)
