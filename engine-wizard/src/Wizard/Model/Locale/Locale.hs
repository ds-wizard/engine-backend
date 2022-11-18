module Wizard.Model.Locale.Locale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Locale =
  Locale
    { _localeUuid :: U.UUID
    , _localeName :: String
    , _localeCode :: String
    , _localeFallback :: Bool
    , _localeEnabled :: Bool
    , _localeAppUuid :: U.UUID
    , _localeCreatedAt :: UTCTime
    , _localeUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
