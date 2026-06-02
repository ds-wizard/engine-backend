module Wizard.Model.User.UserProfile where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.Time
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
  , pluginSettings :: M.Map U.UUID A.Value
  , emailVerifiedAt :: Maybe UTCTime
  , emailPending :: Maybe String
  }
  deriving (Show, Eq, Generic)
