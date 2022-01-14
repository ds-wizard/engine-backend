module Shared.Model.Event.Integration.IntegrationEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Integration.IntegrationEvent

instance HasUuid' AddIntegrationEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddIntegrationEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddIntegrationEvent -> U.UUID -> AddIntegrationEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditIntegrationEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditIntegrationEvent -> U.UUID -> EditIntegrationEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteIntegrationEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteIntegrationEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteIntegrationEvent -> U.UUID -> DeleteIntegrationEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddIntegrationEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddIntegrationEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddIntegrationEvent -> U.UUID -> AddIntegrationEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditIntegrationEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditIntegrationEvent -> U.UUID -> EditIntegrationEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteIntegrationEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteIntegrationEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteIntegrationEvent -> U.UUID -> DeleteIntegrationEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddIntegrationEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddIntegrationEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddIntegrationEvent -> U.UUID -> AddIntegrationEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditIntegrationEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditIntegrationEvent -> U.UUID -> EditIntegrationEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteIntegrationEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteIntegrationEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteIntegrationEvent -> U.UUID -> DeleteIntegrationEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddIntegrationEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddIntegrationEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: AddIntegrationEvent -> UTCTime -> AddIntegrationEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' EditIntegrationEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: EditIntegrationEvent -> UTCTime -> EditIntegrationEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' DeleteIntegrationEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteIntegrationEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteIntegrationEvent -> UTCTime -> DeleteIntegrationEvent
      set entity newValue = entity & createdAt .~ newValue
