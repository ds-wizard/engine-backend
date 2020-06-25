module Shared.Model.Event.Tag.TagEventLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Tag.TagEvent

instance HasUuid' AddTagEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddTagEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddTagEvent -> U.UUID -> AddTagEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditTagEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditTagEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditTagEvent -> U.UUID -> EditTagEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteTagEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteTagEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteTagEvent -> U.UUID -> DeleteTagEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddTagEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddTagEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddTagEvent -> U.UUID -> AddTagEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditTagEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditTagEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditTagEvent -> U.UUID -> EditTagEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteTagEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteTagEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteTagEvent -> U.UUID -> DeleteTagEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddTagEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddTagEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddTagEvent -> U.UUID -> AddTagEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditTagEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditTagEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditTagEvent -> U.UUID -> EditTagEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteTagEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteTagEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteTagEvent -> U.UUID -> DeleteTagEvent
      set entity newValue = entity & entityUuid .~ newValue
