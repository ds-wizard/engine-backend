module Registry.Model.UserEmailLink.UserEmailLinkType where

import GHC.Generics

data UserEmailLinkType
  = RegistrationUserEmailLinkType
  | ForgottenTokenUserEmailLinkType
  deriving (Show, Eq, Generic, Read)
