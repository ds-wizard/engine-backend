module Shared.Model.Event.Metric.MetricEventLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Metric.MetricEvent

instance HasUuid' AddMetricEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddMetricEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddMetricEvent -> U.UUID -> AddMetricEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditMetricEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditMetricEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditMetricEvent -> U.UUID -> EditMetricEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteMetricEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteMetricEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteMetricEvent -> U.UUID -> DeleteMetricEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddMetricEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddMetricEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddMetricEvent -> U.UUID -> AddMetricEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditMetricEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditMetricEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditMetricEvent -> U.UUID -> EditMetricEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteMetricEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteMetricEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteMetricEvent -> U.UUID -> DeleteMetricEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddMetricEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddMetricEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddMetricEvent -> U.UUID -> AddMetricEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditMetricEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditMetricEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditMetricEvent -> U.UUID -> EditMetricEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteMetricEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteMetricEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteMetricEvent -> U.UUID -> DeleteMetricEvent
      set entity newValue = entity & entityUuid .~ newValue
