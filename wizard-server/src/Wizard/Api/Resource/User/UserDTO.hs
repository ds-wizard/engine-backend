module Wizard.Api.Resource.User.UserDTO where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropJM ()
import Wizard.Util.Hashable ()

data UserDTO = UserDTO
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , sources :: [String]
  , uRole :: String
  , permissions :: [String]
  , active :: Bool
  , imageUrl :: Maybe String
  , locale :: Maybe U.UUID
  , lastSeenNewsId :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserDTO where
  a == b =
    a.uuid == b.uuid
      && a.firstName == b.firstName
      && a.lastName == b.lastName
      && a.email == b.email
      && a.affiliation == b.affiliation
      && a.sources == b.sources
      && a.uRole == b.uRole
      && a.permissions == b.permissions
      && a.active == b.active
      && a.imageUrl == b.imageUrl
      && a.locale == b.locale
      && a.lastSeenNewsId == b.lastSeenNewsId

instance Hashable UserDTO
