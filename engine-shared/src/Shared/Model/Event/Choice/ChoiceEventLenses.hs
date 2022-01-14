module Shared.Model.Event.Choice.ChoiceEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Choice.ChoiceEvent

instance HasUuid' AddChoiceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChoiceEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddChoiceEvent -> U.UUID -> AddChoiceEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditChoiceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChoiceEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditChoiceEvent -> U.UUID -> EditChoiceEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteChoiceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChoiceEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteChoiceEvent -> U.UUID -> DeleteChoiceEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddChoiceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChoiceEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddChoiceEvent -> U.UUID -> AddChoiceEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditChoiceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChoiceEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditChoiceEvent -> U.UUID -> EditChoiceEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteChoiceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChoiceEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteChoiceEvent -> U.UUID -> DeleteChoiceEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddChoiceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChoiceEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddChoiceEvent -> U.UUID -> AddChoiceEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditChoiceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChoiceEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditChoiceEvent -> U.UUID -> EditChoiceEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteChoiceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChoiceEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteChoiceEvent -> U.UUID -> DeleteChoiceEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddChoiceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChoiceEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: AddChoiceEvent -> UTCTime -> AddChoiceEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' EditChoiceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChoiceEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: EditChoiceEvent -> UTCTime -> EditChoiceEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' DeleteChoiceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChoiceEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteChoiceEvent -> UTCTime -> DeleteChoiceEvent
      set entity newValue = entity & createdAt .~ newValue
