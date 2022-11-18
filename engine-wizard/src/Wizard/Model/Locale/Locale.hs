module Wizard.Model.Locale.Locale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Locale = Locale
  { uuid :: U.UUID
  , name :: String
  , code :: String
  , fallback :: Bool
  , enabled :: Bool
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
