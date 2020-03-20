module Shared.Service.Metric.MetricMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.KnowledgeModelMapper

toMetricDTO :: Metric -> MetricDTO
toMetricDTO m =
  MetricDTO
    { _metricDTOUuid = m ^. uuid
    , _metricDTOTitle = m ^. title
    , _metricDTOAbbreviation = m ^. abbreviation
    , _metricDTODescription = m ^. description
    , _metricDTOReferences = toReferenceDTO <$> m ^. references
    , _metricDTOCreatedAt = m ^. createdAt
    , _metricDTOUpdatedAt = m ^. updatedAt
    }
