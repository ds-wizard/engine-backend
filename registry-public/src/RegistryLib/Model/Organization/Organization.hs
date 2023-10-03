module RegistryLib.Model.Organization.Organization where

import Data.Time
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationRole

data Organization = Organization
  { organizationId :: String
  , name :: String
  , description :: String
  , email :: String
  , oRole :: OrganizationRole
  , token :: String
  , active :: Bool
  , logo :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq Organization where
  a == b =
    organizationId a == organizationId b
      && name a == name b
      && description a == description b
      && email a == email b
      && oRole a == oRole b
      && token a == token b
      && active a == active b
      && logo a == logo b
