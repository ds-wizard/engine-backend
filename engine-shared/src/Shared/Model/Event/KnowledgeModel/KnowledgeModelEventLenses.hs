module Shared.Model.Event.KnowledgeModel.KnowledgeModelEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent

instance HasUuid' AddKnowledgeModelEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddKnowledgeModelEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddKnowledgeModelEvent -> U.UUID -> AddKnowledgeModelEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditKnowledgeModelEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditKnowledgeModelEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditKnowledgeModelEvent -> U.UUID -> EditKnowledgeModelEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddKnowledgeModelEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddKnowledgeModelEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddKnowledgeModelEvent -> U.UUID -> AddKnowledgeModelEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditKnowledgeModelEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditKnowledgeModelEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditKnowledgeModelEvent -> U.UUID -> EditKnowledgeModelEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddKnowledgeModelEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddKnowledgeModelEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddKnowledgeModelEvent -> U.UUID -> AddKnowledgeModelEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditKnowledgeModelEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditKnowledgeModelEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditKnowledgeModelEvent -> U.UUID -> EditKnowledgeModelEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddKnowledgeModelEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddKnowledgeModelEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: AddKnowledgeModelEvent -> UTCTime -> AddKnowledgeModelEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' EditKnowledgeModelEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditKnowledgeModelEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: EditKnowledgeModelEvent -> UTCTime -> EditKnowledgeModelEvent
      set entity newValue = entity & createdAt .~ newValue
