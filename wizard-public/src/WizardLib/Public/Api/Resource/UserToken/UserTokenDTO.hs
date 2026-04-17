module WizardLib.Public.Api.Resource.UserToken.UserTokenDTO where

import Data.Time
import GHC.Generics

data UserTokenDTO
  = CodeRequiredDTO
  | ConsentsRequiredDTO
      { hash :: String
      }
  | IdentityLinkedDTO
  | UserTokenDTO
      { token :: String
      , expiresAt :: UTCTime
      }
  deriving (Show, Eq, Generic)
