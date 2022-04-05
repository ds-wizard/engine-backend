module Shared.Model.Event.Integration.IntegrationEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.EventField
import Shared.Model.Event.Integration.IntegrationEvent

instance HasUuid' AddIntegrationEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddIntegrationEvent -> U.UUID
      get (AddApiIntegrationEvent' entity) = entity ^. uuid
      get (AddWidgetIntegrationEvent' entity) = entity ^. uuid
      set :: AddIntegrationEvent -> U.UUID -> AddIntegrationEvent
      set (AddApiIntegrationEvent' entity) newUuid = AddApiIntegrationEvent' $ entity & uuid .~ newUuid
      set (AddWidgetIntegrationEvent' entity) newUuid = AddWidgetIntegrationEvent' $ entity & uuid .~ newUuid

instance HasUuid' EditIntegrationEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> U.UUID
      get (EditApiIntegrationEvent' entity) = entity ^. uuid
      get (EditWidgetIntegrationEvent' entity) = entity ^. uuid
      set :: EditIntegrationEvent -> U.UUID -> EditIntegrationEvent
      set (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity & uuid .~ newValue
      set (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity & uuid .~ newValue

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
      get (AddApiIntegrationEvent' entity) = entity ^. parentUuid
      get (AddWidgetIntegrationEvent' entity) = entity ^. parentUuid
      set :: AddIntegrationEvent -> U.UUID -> AddIntegrationEvent
      set (AddApiIntegrationEvent' entity) newUuid = AddApiIntegrationEvent' $ entity & parentUuid .~ newUuid
      set (AddWidgetIntegrationEvent' entity) newUuid = AddWidgetIntegrationEvent' $ entity & parentUuid .~ newUuid

instance HasParentUuid' EditIntegrationEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> U.UUID
      get (EditApiIntegrationEvent' entity) = entity ^. parentUuid
      get (EditWidgetIntegrationEvent' entity) = entity ^. parentUuid
      set :: EditIntegrationEvent -> U.UUID -> EditIntegrationEvent
      set (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity & parentUuid .~ newValue
      set (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity & parentUuid .~ newValue

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
      get (AddApiIntegrationEvent' entity) = entity ^. entityUuid
      get (AddWidgetIntegrationEvent' entity) = entity ^. entityUuid
      set :: AddIntegrationEvent -> U.UUID -> AddIntegrationEvent
      set (AddApiIntegrationEvent' entity) newUuid = AddApiIntegrationEvent' $ entity & entityUuid .~ newUuid
      set (AddWidgetIntegrationEvent' entity) newUuid = AddWidgetIntegrationEvent' $ entity & entityUuid .~ newUuid

instance HasEntityUuid' EditIntegrationEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> U.UUID
      get (EditApiIntegrationEvent' entity) = entity ^. entityUuid
      get (EditWidgetIntegrationEvent' entity) = entity ^. entityUuid
      set :: EditIntegrationEvent -> U.UUID -> EditIntegrationEvent
      set (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity & entityUuid .~ newValue
      set (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteIntegrationEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteIntegrationEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteIntegrationEvent -> U.UUID -> DeleteIntegrationEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasProps' EditIntegrationEvent (EventField [String]) where
  props' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> EventField [String]
      get (EditApiIntegrationEvent' e) = e ^. props
      get (EditWidgetIntegrationEvent' e) = e ^. props
      set :: EditIntegrationEvent -> EventField [String] -> EditIntegrationEvent
      set (EditApiIntegrationEvent' e) newValue = EditApiIntegrationEvent' $ e & props .~ newValue
      set (EditWidgetIntegrationEvent' e) newValue = EditWidgetIntegrationEvent' $ e & props .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddIntegrationEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddIntegrationEvent -> UTCTime
      get (AddApiIntegrationEvent' entity) = entity ^. createdAt
      get (AddWidgetIntegrationEvent' entity) = entity ^. createdAt
      set :: AddIntegrationEvent -> UTCTime -> AddIntegrationEvent
      set (AddApiIntegrationEvent' entity) newUuid = AddApiIntegrationEvent' $ entity & createdAt .~ newUuid
      set (AddWidgetIntegrationEvent' entity) newUuid = AddWidgetIntegrationEvent' $ entity & createdAt .~ newUuid

instance HasCreatedAt' EditIntegrationEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditIntegrationEvent -> UTCTime
      get (EditApiIntegrationEvent' entity) = entity ^. createdAt
      get (EditWidgetIntegrationEvent' entity) = entity ^. createdAt
      set :: EditIntegrationEvent -> UTCTime -> EditIntegrationEvent
      set (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity & createdAt .~ newValue
      set (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity & createdAt .~ newValue

instance HasCreatedAt' DeleteIntegrationEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteIntegrationEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteIntegrationEvent -> UTCTime -> DeleteIntegrationEvent
      set entity newValue = entity & createdAt .~ newValue
