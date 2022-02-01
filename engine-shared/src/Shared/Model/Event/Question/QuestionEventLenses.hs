module Shared.Model.Event.Question.QuestionEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.EventField
import Shared.Model.Event.Question.QuestionEvent

instance HasUuid' AddQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> U.UUID
      get (AddOptionsQuestionEvent' entity) = entity ^. uuid
      get (AddMultiChoiceQuestionEvent' entity) = entity ^. uuid
      get (AddListQuestionEvent' entity) = entity ^. uuid
      get (AddValueQuestionEvent' entity) = entity ^. uuid
      get (AddIntegrationQuestionEvent' entity) = entity ^. uuid
      set :: AddQuestionEvent -> U.UUID -> AddQuestionEvent
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & uuid .~ newValue
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & uuid .~ newValue
      set (AddMultiChoiceQuestionEvent' entity) newValue = AddMultiChoiceQuestionEvent' $ entity & uuid .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & uuid .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity & uuid .~ newValue

instance HasUuid' EditQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> U.UUID
      get (EditOptionsQuestionEvent' entity) = entity ^. uuid
      get (EditMultiChoiceQuestionEvent' entity) = entity ^. uuid
      get (EditListQuestionEvent' entity) = entity ^. uuid
      get (EditValueQuestionEvent' entity) = entity ^. uuid
      get (EditIntegrationQuestionEvent' entity) = entity ^. uuid
      set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & uuid .~ newValue
      set (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity & uuid .~ newValue
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & uuid .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & uuid .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity & uuid .~ newValue

instance HasUuid' DeleteQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteQuestionEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteQuestionEvent -> U.UUID -> DeleteQuestionEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddQuestionEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> U.UUID
      get (AddOptionsQuestionEvent' entity) = entity ^. parentUuid
      get (AddMultiChoiceQuestionEvent' entity) = entity ^. parentUuid
      get (AddListQuestionEvent' entity) = entity ^. parentUuid
      get (AddValueQuestionEvent' entity) = entity ^. parentUuid
      get (AddIntegrationQuestionEvent' entity) = entity ^. parentUuid
      set :: AddQuestionEvent -> U.UUID -> AddQuestionEvent
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddMultiChoiceQuestionEvent' entity) newValue =
        AddMultiChoiceQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue =
        AddIntegrationQuestionEvent' $ entity & parentUuid .~ newValue

instance HasParentUuid' EditQuestionEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> U.UUID
      get (EditOptionsQuestionEvent' entity) = entity ^. parentUuid
      get (EditMultiChoiceQuestionEvent' entity) = entity ^. parentUuid
      get (EditListQuestionEvent' entity) = entity ^. parentUuid
      get (EditValueQuestionEvent' entity) = entity ^. parentUuid
      get (EditIntegrationQuestionEvent' entity) = entity ^. parentUuid
      set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & parentUuid .~ newValue
      set (EditMultiChoiceQuestionEvent' entity) newValue =
        EditMultiChoiceQuestionEvent' $ entity & parentUuid .~ newValue
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & parentUuid .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & parentUuid .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue =
        EditIntegrationQuestionEvent' $ entity & parentUuid .~ newValue

instance HasParentUuid' DeleteQuestionEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteQuestionEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteQuestionEvent -> U.UUID -> DeleteQuestionEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddQuestionEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> U.UUID
      get (AddOptionsQuestionEvent' entity) = entity ^. entityUuid
      get (AddMultiChoiceQuestionEvent' entity) = entity ^. entityUuid
      get (AddListQuestionEvent' entity) = entity ^. entityUuid
      get (AddValueQuestionEvent' entity) = entity ^. entityUuid
      get (AddIntegrationQuestionEvent' entity) = entity ^. entityUuid
      set :: AddQuestionEvent -> U.UUID -> AddQuestionEvent
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddMultiChoiceQuestionEvent' entity) newValue =
        AddMultiChoiceQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue =
        AddIntegrationQuestionEvent' $ entity & entityUuid .~ newValue

instance HasEntityUuid' EditQuestionEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> U.UUID
      get (EditOptionsQuestionEvent' entity) = entity ^. entityUuid
      get (EditMultiChoiceQuestionEvent' entity) = entity ^. entityUuid
      get (EditListQuestionEvent' entity) = entity ^. entityUuid
      get (EditValueQuestionEvent' entity) = entity ^. entityUuid
      get (EditIntegrationQuestionEvent' entity) = entity ^. entityUuid
      set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & entityUuid .~ newValue
      set (EditMultiChoiceQuestionEvent' entity) newValue =
        EditMultiChoiceQuestionEvent' $ entity & entityUuid .~ newValue
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & entityUuid .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & entityUuid .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue =
        EditIntegrationQuestionEvent' $ entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteQuestionEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteQuestionEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteQuestionEvent -> U.UUID -> DeleteQuestionEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddQuestionEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> UTCTime
      get (AddOptionsQuestionEvent' entity) = entity ^. createdAt
      get (AddMultiChoiceQuestionEvent' entity) = entity ^. createdAt
      get (AddListQuestionEvent' entity) = entity ^. createdAt
      get (AddValueQuestionEvent' entity) = entity ^. createdAt
      get (AddIntegrationQuestionEvent' entity) = entity ^. createdAt
      set :: AddQuestionEvent -> UTCTime -> AddQuestionEvent
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & createdAt .~ newValue
      set (AddMultiChoiceQuestionEvent' entity) newValue = AddMultiChoiceQuestionEvent' $ entity & createdAt .~ newValue
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & createdAt .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & createdAt .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity & createdAt .~ newValue

instance HasCreatedAt' EditQuestionEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> UTCTime
      get (EditOptionsQuestionEvent' entity) = entity ^. createdAt
      get (EditMultiChoiceQuestionEvent' entity) = entity ^. createdAt
      get (EditListQuestionEvent' entity) = entity ^. createdAt
      get (EditValueQuestionEvent' entity) = entity ^. createdAt
      get (EditIntegrationQuestionEvent' entity) = entity ^. createdAt
      set :: EditQuestionEvent -> UTCTime -> EditQuestionEvent
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & createdAt .~ newValue
      set (EditMultiChoiceQuestionEvent' entity) newValue =
        EditMultiChoiceQuestionEvent' $ entity & createdAt .~ newValue
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & createdAt .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & createdAt .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue =
        EditIntegrationQuestionEvent' $ entity & createdAt .~ newValue

instance HasCreatedAt' DeleteQuestionEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteQuestionEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteQuestionEvent -> UTCTime -> DeleteQuestionEvent
      set entity newValue = entity & createdAt .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasExpertUuids' EditQuestionEvent (EventField [U.UUID]) where
  expertUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> EventField [U.UUID]
      get (EditOptionsQuestionEvent' entity) = entity ^. expertUuids
      get (EditMultiChoiceQuestionEvent' entity) = entity ^. expertUuids
      get (EditListQuestionEvent' entity) = entity ^. expertUuids
      get (EditValueQuestionEvent' entity) = entity ^. expertUuids
      get (EditIntegrationQuestionEvent' entity) = entity ^. expertUuids
      set :: EditQuestionEvent -> EventField [U.UUID] -> EditQuestionEvent
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & expertUuids .~ newValue
      set (EditMultiChoiceQuestionEvent' entity) newValue =
        EditMultiChoiceQuestionEvent' $ entity & expertUuids .~ newValue
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & expertUuids .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & expertUuids .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue =
        EditIntegrationQuestionEvent' $ entity & expertUuids .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasReferenceUuids' EditQuestionEvent (EventField [U.UUID]) where
  referenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> EventField [U.UUID]
      get (EditOptionsQuestionEvent' entity) = entity ^. referenceUuids
      get (EditMultiChoiceQuestionEvent' entity) = entity ^. referenceUuids
      get (EditListQuestionEvent' entity) = entity ^. referenceUuids
      get (EditValueQuestionEvent' entity) = entity ^. referenceUuids
      get (EditIntegrationQuestionEvent' entity) = entity ^. referenceUuids
      set :: EditQuestionEvent -> EventField [U.UUID] -> EditQuestionEvent
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & referenceUuids .~ newValue
      set (EditMultiChoiceQuestionEvent' entity) newValue =
        EditMultiChoiceQuestionEvent' $ entity & referenceUuids .~ newValue
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & referenceUuids .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & referenceUuids .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue =
        EditIntegrationQuestionEvent' $ entity & referenceUuids .~ newValue
