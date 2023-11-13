module WizardLib.Public.Api.Resource.User.UserWithMembershipDTO where

import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.UserGroupMembership

data UserWithMembershipDTO = UserWithMembershipDTO
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , gravatarHash :: String
  , imageUrl :: Maybe String
  , membershipType :: UserGroupMembershipType
  }
  deriving (Show, Eq, Generic)
