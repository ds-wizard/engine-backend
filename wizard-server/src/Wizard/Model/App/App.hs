module Wizard.Model.App.App where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data App = App
  { uuid :: U.UUID
  , appId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
