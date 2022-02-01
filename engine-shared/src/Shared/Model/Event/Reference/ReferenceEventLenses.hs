module Shared.Model.Event.Reference.ReferenceEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Reference.ReferenceEvent

instance HasUuid' AddReferenceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddReferenceEvent -> U.UUID
      get (AddResourcePageReferenceEvent' entity) = entity ^. uuid
      get (AddURLReferenceEvent' entity) = entity ^. uuid
      get (AddCrossReferenceEvent' entity) = entity ^. uuid
      set :: AddReferenceEvent -> U.UUID -> AddReferenceEvent
      set (AddResourcePageReferenceEvent' entity) newCross = AddResourcePageReferenceEvent' $ entity & uuid .~ newCross
      set (AddURLReferenceEvent' entity) newCross = AddURLReferenceEvent' $ entity & uuid .~ newCross
      set (AddCrossReferenceEvent' entity) newCross = AddCrossReferenceEvent' $ entity & uuid .~ newCross

instance HasUuid' EditReferenceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditReferenceEvent -> U.UUID
      get (EditResourcePageReferenceEvent' entity) = entity ^. uuid
      get (EditURLReferenceEvent' entity) = entity ^. uuid
      get (EditCrossReferenceEvent' entity) = entity ^. uuid
      set :: EditReferenceEvent -> U.UUID -> EditReferenceEvent
      set (EditResourcePageReferenceEvent' entity) newCross =
        EditResourcePageReferenceEvent' $ entity & uuid .~ newCross
      set (EditURLReferenceEvent' entity) newCross = EditURLReferenceEvent' $ entity & uuid .~ newCross
      set (EditCrossReferenceEvent' entity) newCross = EditCrossReferenceEvent' $ entity & uuid .~ newCross

instance HasUuid' DeleteReferenceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteReferenceEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteReferenceEvent -> U.UUID -> DeleteReferenceEvent
      set entity newCross = entity & uuid .~ newCross

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddReferenceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddReferenceEvent -> U.UUID
      get (AddResourcePageReferenceEvent' entity) = entity ^. parentUuid
      get (AddURLReferenceEvent' entity) = entity ^. parentUuid
      get (AddCrossReferenceEvent' entity) = entity ^. parentUuid
      set :: AddReferenceEvent -> U.UUID -> AddReferenceEvent
      set (AddResourcePageReferenceEvent' entity) newCross =
        AddResourcePageReferenceEvent' $ entity & parentUuid .~ newCross
      set (AddURLReferenceEvent' entity) newCross = AddURLReferenceEvent' $ entity & parentUuid .~ newCross
      set (AddCrossReferenceEvent' entity) newCross = AddCrossReferenceEvent' $ entity & parentUuid .~ newCross

instance HasParentUuid' EditReferenceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditReferenceEvent -> U.UUID
      get (EditResourcePageReferenceEvent' entity) = entity ^. parentUuid
      get (EditURLReferenceEvent' entity) = entity ^. parentUuid
      get (EditCrossReferenceEvent' entity) = entity ^. parentUuid
      set :: EditReferenceEvent -> U.UUID -> EditReferenceEvent
      set (EditResourcePageReferenceEvent' entity) newCross =
        EditResourcePageReferenceEvent' $ entity & parentUuid .~ newCross
      set (EditURLReferenceEvent' entity) newCross = EditURLReferenceEvent' $ entity & parentUuid .~ newCross
      set (EditCrossReferenceEvent' entity) newCross = EditCrossReferenceEvent' $ entity & parentUuid .~ newCross

instance HasParentUuid' DeleteReferenceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteReferenceEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteReferenceEvent -> U.UUID -> DeleteReferenceEvent
      set entity newCross = entity & parentUuid .~ newCross

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddReferenceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddReferenceEvent -> U.UUID
      get (AddResourcePageReferenceEvent' entity) = entity ^. entityUuid
      get (AddURLReferenceEvent' entity) = entity ^. entityUuid
      get (AddCrossReferenceEvent' entity) = entity ^. entityUuid
      set :: AddReferenceEvent -> U.UUID -> AddReferenceEvent
      set (AddResourcePageReferenceEvent' entity) newCross =
        AddResourcePageReferenceEvent' $ entity & entityUuid .~ newCross
      set (AddURLReferenceEvent' entity) newCross = AddURLReferenceEvent' $ entity & entityUuid .~ newCross
      set (AddCrossReferenceEvent' entity) newCross = AddCrossReferenceEvent' $ entity & entityUuid .~ newCross

instance HasEntityUuid' EditReferenceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditReferenceEvent -> U.UUID
      get (EditResourcePageReferenceEvent' entity) = entity ^. entityUuid
      get (EditURLReferenceEvent' entity) = entity ^. entityUuid
      get (EditCrossReferenceEvent' entity) = entity ^. entityUuid
      set :: EditReferenceEvent -> U.UUID -> EditReferenceEvent
      set (EditResourcePageReferenceEvent' entity) newCross =
        EditResourcePageReferenceEvent' $ entity & entityUuid .~ newCross
      set (EditURLReferenceEvent' entity) newCross = EditURLReferenceEvent' $ entity & entityUuid .~ newCross
      set (EditCrossReferenceEvent' entity) newCross = EditCrossReferenceEvent' $ entity & entityUuid .~ newCross

instance HasEntityUuid' DeleteReferenceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteReferenceEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteReferenceEvent -> U.UUID -> DeleteReferenceEvent
      set entity newCross = entity & entityUuid .~ newCross

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddReferenceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddReferenceEvent -> UTCTime
      get (AddResourcePageReferenceEvent' entity) = entity ^. createdAt
      get (AddURLReferenceEvent' entity) = entity ^. createdAt
      get (AddCrossReferenceEvent' entity) = entity ^. createdAt
      set :: AddReferenceEvent -> UTCTime -> AddReferenceEvent
      set (AddResourcePageReferenceEvent' entity) newCross =
        AddResourcePageReferenceEvent' $ entity & createdAt .~ newCross
      set (AddURLReferenceEvent' entity) newCross = AddURLReferenceEvent' $ entity & createdAt .~ newCross
      set (AddCrossReferenceEvent' entity) newCross = AddCrossReferenceEvent' $ entity & createdAt .~ newCross

instance HasCreatedAt' EditReferenceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditReferenceEvent -> UTCTime
      get (EditResourcePageReferenceEvent' entity) = entity ^. createdAt
      get (EditURLReferenceEvent' entity) = entity ^. createdAt
      get (EditCrossReferenceEvent' entity) = entity ^. createdAt
      set :: EditReferenceEvent -> UTCTime -> EditReferenceEvent
      set (EditResourcePageReferenceEvent' entity) newCross =
        EditResourcePageReferenceEvent' $ entity & createdAt .~ newCross
      set (EditURLReferenceEvent' entity) newCross = EditURLReferenceEvent' $ entity & createdAt .~ newCross
      set (EditCrossReferenceEvent' entity) newCross = EditCrossReferenceEvent' $ entity & createdAt .~ newCross

instance HasCreatedAt' DeleteReferenceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteReferenceEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteReferenceEvent -> UTCTime -> DeleteReferenceEvent
      set entity newCross = entity & createdAt .~ newCross
