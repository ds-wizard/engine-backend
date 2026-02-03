module Wizard.Model.User.UserPluginSettings where

import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserPluginSettings = UserPluginSettings
  { userUuid :: U.UUID
  , pluginUuid :: U.UUID
  , values :: A.Value
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
