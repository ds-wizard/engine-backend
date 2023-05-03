module Wizard.Model.ActionKey.ActionKeyType where

import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  | TwoFactorAuthActionKey
  deriving (Show, Eq, Generic, Read)
