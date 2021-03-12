{-# LANGUAGE StandaloneDeriving #-}

module Wizard.Util.Hashable where

import Data.Hashable
import Data.Time
import GHC.Generics

deriving instance Generic Day

instance Hashable Day

deriving instance Generic LocalTime

instance Hashable LocalTime

deriving instance Generic TimeOfDay

instance Hashable TimeOfDay

deriving instance Generic TimeZone

instance Hashable TimeZone

instance Hashable DiffTime where
  hashWithSalt s = (hashWithSalt s :: Double -> Int) . realToFrac

deriving instance Generic UTCTime

instance Hashable UTCTime

deriving instance Generic ZonedTime

instance Hashable ZonedTime
