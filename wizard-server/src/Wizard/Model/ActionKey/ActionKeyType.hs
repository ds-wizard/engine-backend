module Wizard.Model.ActionKey.ActionKeyType where

import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  | TwoFactorAuthActionKey
  | ConsentsRequiredActionKey
  | EmailChangeActionKey
  deriving (Show, Eq, Generic, Read)
