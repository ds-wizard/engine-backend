module Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

metricF :: Metric
metricF =
  Metric
    { uuid = u' "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"
    , title = "Findability"
    , abbreviation = Just "F"
    , description =
        Just
          "The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
    , annotations = []
    }

metricFEdited :: Metric
metricFEdited =
  metricF
    { title = "EDITED: Findability"
    , abbreviation = Just "FE"
    , description =
        Just
          "EDITED: The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

metricA :: Metric
metricA =
  Metric
    { uuid = u' "0feac7e6-add4-4723-abae-be5ce7864c63"
    , title = "Accessibility"
    , abbreviation = Just "A"
    , description =
        Just
          "The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!"
    , annotations = []
    }

metricI :: Metric
metricI =
  Metric
    { uuid = u' "a42bded3-a085-45f8-b384-32b4a77c8385"
    , title = "Interoperability"
    , abbreviation = Just "I"
    , description =
        Just
          "The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and 'understand' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community."
    , annotations = []
    }

metricR :: Metric
metricR =
  Metric
    { uuid = u' "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"
    , title = "Reusability"
    , abbreviation = Just "R"
    , description =
        Just
          "The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent)."
    , annotations = []
    }

metricG :: Metric
metricG =
  Metric
    { uuid = u' "8845fe2b-79df-4138-baea-3a035bf5e249"
    , title = "Good DMP Practice"
    , abbreviation = Just "G"
    , description =
        Just
          "The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea."
    , annotations = []
    }

metricO :: Metric
metricO =
  Metric
    { uuid = u' "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"
    , title = "Openness"
    , abbreviation = Just "O"
    , description =
        Just
          "The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use."
    , annotations = []
    }

mm1 :: MetricMeasure
mm1 =
  MetricMeasure {metricUuid = metricF.uuid, measure = 0.4, weight = 0.5}
