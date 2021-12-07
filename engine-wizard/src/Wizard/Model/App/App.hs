module Wizard.Model.App.App where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data App =
  App
    { _appUuid :: U.UUID
    , _appAppId :: String
    , _appName :: String
    , _appServerDomain :: String
    , _appServerUrl :: String
    , _appClientUrl :: String
    , _appEnabled :: Bool
    , _appCreatedAt :: UTCTime
    , _appUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
