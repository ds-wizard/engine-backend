module WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO where

import qualified Data.UUID as U
import GHC.Generics
import qualified Jose.Jwt as JWT

data UserTokenClaimsDTO = UserTokenClaimsDTO
  { exp :: JWT.IntDate
  , version :: Integer
  , userUuid :: U.UUID
  , tokenUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
