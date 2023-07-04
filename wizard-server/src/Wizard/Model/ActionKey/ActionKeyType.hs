module Wizard.Model.ActionKey.ActionKeyType where

import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  | TwoFactorAuthActionKey
  | ConsentsRequiredActionKey
  deriving (Show, Eq, Generic, Read)
