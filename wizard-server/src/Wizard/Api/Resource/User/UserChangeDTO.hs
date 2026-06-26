module Wizard.Api.Resource.User.UserChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data UserChangeDTO = UserChangeDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , roleUuid :: U.UUID
  , active :: Bool
  }
  deriving (Generic)
