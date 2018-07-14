module Database.Migration.Development.Metric.Data.Metrics where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Model.KnowledgeModel.KnowledgeModel

metricF =
  Metric
  { _metricUuid = fromJust . U.fromString $ "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"
  , _metricTitle = "Findability"
  , _metricAbbreviation = Just "∀"
  , _metricDescription = Just "..."
  , _metricReferences = []
  , _metricCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _metricUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

metricA =
  Metric
  { _metricUuid = fromJust . U.fromString $ "0feac7e6-add4-4723-abae-be5ce7864c63"
  , _metricTitle = "Accessibility"
  , _metricAbbreviation = Just ""
  , _metricDescription = Just ""
  , _metricReferences = []
  , _metricCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _metricUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

metricI =
  Metric
  { _metricUuid = fromJust . U.fromString $ "a42bded3-a085-45f8-b384-32b4a77c8385"
  , _metricTitle = "Interoperability"
  , _metricAbbreviation = Just "I"
  , _metricDescription = Just ""
  , _metricReferences = []
  , _metricCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _metricUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

metricR =
  Metric
  { _metricUuid = fromJust . U.fromString $ "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"
  , _metricTitle = "Reusability"
  , _metricAbbreviation = Just "R"
  , _metricDescription = Just ""
  , _metricReferences = []
  , _metricCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _metricUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

metricG =
  Metric
  { _metricUuid = fromJust . U.fromString $ "8845fe2b-79df-4138-baea-3a035bf5e249"
  , _metricTitle = "Good DMP Practice"
  , _metricAbbreviation = Just ""
  , _metricDescription = Just ""
  , _metricReferences = []
  , _metricCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _metricUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

metricO =
  Metric
  { _metricUuid = fromJust . U.fromString $ "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"
  , _metricTitle = "Openness"
  , _metricAbbreviation = Just "O"
  , _metricDescription = Just ""
  , _metricReferences = []
  , _metricCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _metricUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }
