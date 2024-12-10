{-# LANGUAGE StandaloneDeriving #-}

module WizardLib.Common.Util.Hashable where

import Data.Hashable
import Data.Time
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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

deriving instance Eq ZonedTime

deriving instance Generic ZonedTime

instance Hashable ZonedTime

instance Hashable MetricMeasure
instance Hashable QuestionValidation
instance Hashable QuestionValueType
instance Hashable a => Hashable (EventField a)
instance (Hashable key, Hashable value) => Hashable (MapEntry key value)
