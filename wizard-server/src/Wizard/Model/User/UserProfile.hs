module Wizard.Model.User.UserProfile where

import qualified Data.UUID as U
import GHC.Generics

data UserProfile = UserProfile
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  , uRole :: String
  , permissions :: [String]
  , lastSeenNewsId :: Maybe String
  , userGroupUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
