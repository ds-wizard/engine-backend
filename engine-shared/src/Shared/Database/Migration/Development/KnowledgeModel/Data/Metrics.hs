module Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

metricF :: Metric
metricF =
  Metric
    { _metricUuid = u' "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"
    , _metricTitle = "Findability"
    , _metricAbbreviation = Just "F"
    , _metricDescription =
        Just
          "The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
    , _metricAnnotations = M.empty
    }

metricFEdited :: Metric
metricFEdited =
  metricF
    { _metricTitle = "EDITED: Findability"
    , _metricAbbreviation = Just "FE"
    , _metricDescription =
        Just
          "EDITED: The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
    , _metricAnnotations = M.fromList [("newAnnotation", "someValue")]
    }

metricA :: Metric
metricA =
  Metric
    { _metricUuid = u' "0feac7e6-add4-4723-abae-be5ce7864c63"
    , _metricTitle = "Accessibility"
    , _metricAbbreviation = Just "A"
    , _metricDescription =
        Just
          "The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!"
    , _metricAnnotations = M.empty
    }

metricI :: Metric
metricI =
  Metric
    { _metricUuid = u' "a42bded3-a085-45f8-b384-32b4a77c8385"
    , _metricTitle = "Interoperability"
    , _metricAbbreviation = Just "I"
    , _metricDescription =
        Just
          "The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and 'understand' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community."
    , _metricAnnotations = M.empty
    }

metricR :: Metric
metricR =
  Metric
    { _metricUuid = u' "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"
    , _metricTitle = "Reusability"
    , _metricAbbreviation = Just "R"
    , _metricDescription =
        Just
          "The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent)."
    , _metricAnnotations = M.empty
    }

metricG :: Metric
metricG =
  Metric
    { _metricUuid = u' "8845fe2b-79df-4138-baea-3a035bf5e249"
    , _metricTitle = "Good DMP Practice"
    , _metricAbbreviation = Just "G"
    , _metricDescription =
        Just
          "The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea."
    , _metricAnnotations = M.empty
    }

metricO :: Metric
metricO =
  Metric
    { _metricUuid = u' "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"
    , _metricTitle = "Openness"
    , _metricAbbreviation = Just "O"
    , _metricDescription =
        Just
          "The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use."
    , _metricAnnotations = M.empty
    }

mm1 :: MetricMeasure
mm1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricF ^. uuid
    , _metricMeasureMeasure = 0.4
    , _metricMeasureWeight = 0.5
    , _metricMeasureAnnotations = M.empty
    }
