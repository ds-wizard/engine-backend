module Wizard.Model.UserEmailLink.UserEmailLinkType where

import GHC.Generics

data UserEmailLinkType
  = RegistrationUserEmailLinkType
  | ForgottenPasswordUserEmailLinkType
  | TwoFactorAuthUserEmailLinkType
  | ConsentsRequiredUserEmailLinkType
  | EmailChangeUserEmailLinkType
  deriving (Show, Eq, Generic, Read)
