module Shared.Model.Event.Phase.PhaseEventLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Phase.PhaseEvent

instance HasUuid' AddPhaseEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddPhaseEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddPhaseEvent -> U.UUID -> AddPhaseEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditPhaseEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditPhaseEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditPhaseEvent -> U.UUID -> EditPhaseEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeletePhaseEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeletePhaseEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeletePhaseEvent -> U.UUID -> DeletePhaseEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddPhaseEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddPhaseEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddPhaseEvent -> U.UUID -> AddPhaseEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditPhaseEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditPhaseEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditPhaseEvent -> U.UUID -> EditPhaseEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeletePhaseEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeletePhaseEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeletePhaseEvent -> U.UUID -> DeletePhaseEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddPhaseEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddPhaseEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddPhaseEvent -> U.UUID -> AddPhaseEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditPhaseEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditPhaseEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditPhaseEvent -> U.UUID -> EditPhaseEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeletePhaseEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeletePhaseEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeletePhaseEvent -> U.UUID -> DeletePhaseEvent
      set entity newValue = entity & entityUuid .~ newValue
