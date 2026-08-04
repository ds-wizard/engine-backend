module Wizard.Api.Resource.User.UserCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data UserCreateDTO = UserCreateDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , roleUuid :: Maybe U.UUID
  , password :: String
  }
  deriving (Generic)
