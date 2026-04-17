module WizardLib.Public.Api.Resource.UserToken.UserTokenDTO where

import Data.Time
import GHC.Generics

data UserTokenDTO
  = CodeRequiredDTO
  | ConsentsRequiredDTO
      { hash :: String
      }
  | CompleteRegistrationRequiredDTO
      { hash :: String
      , email :: Maybe String
      , firstName :: Maybe String
      , lastName :: Maybe String
      , imageUrl :: Maybe String
      }
  | IdentityLinkedDTO
  | UserTokenDTO
      { token :: String
      , expiresAt :: UTCTime
      }
  | EmailVerificationRequiredDTO
  deriving (Show, Eq, Generic)
