module Shared.Model.Event.Expert.ExpertEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Expert.ExpertEvent

instance HasUuid' AddExpertEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddExpertEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddExpertEvent -> U.UUID -> AddExpertEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditExpertEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditExpertEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditExpertEvent -> U.UUID -> EditExpertEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteExpertEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteExpertEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteExpertEvent -> U.UUID -> DeleteExpertEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddExpertEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddExpertEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddExpertEvent -> U.UUID -> AddExpertEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditExpertEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditExpertEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditExpertEvent -> U.UUID -> EditExpertEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteExpertEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteExpertEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteExpertEvent -> U.UUID -> DeleteExpertEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddExpertEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddExpertEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddExpertEvent -> U.UUID -> AddExpertEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditExpertEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditExpertEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditExpertEvent -> U.UUID -> EditExpertEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteExpertEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteExpertEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteExpertEvent -> U.UUID -> DeleteExpertEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddExpertEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddExpertEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: AddExpertEvent -> UTCTime -> AddExpertEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' EditExpertEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditExpertEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: EditExpertEvent -> UTCTime -> EditExpertEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' DeleteExpertEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteExpertEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteExpertEvent -> UTCTime -> DeleteExpertEvent
      set entity newValue = entity & createdAt .~ newValue
