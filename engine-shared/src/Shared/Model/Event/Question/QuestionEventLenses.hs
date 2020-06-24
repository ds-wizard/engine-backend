module Shared.Model.Event.Question.QuestionEventLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Question.QuestionEvent

instance HasUuid' AddQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> U.UUID
      get (AddListQuestionEvent' entity) = entity ^. uuid
      get (AddOptionsQuestionEvent' entity) = entity ^. uuid
      get (AddValueQuestionEvent' entity) = entity ^. uuid
      get (AddIntegrationQuestionEvent' entity) = entity ^. uuid
      set :: AddQuestionEvent -> U.UUID -> AddQuestionEvent
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & uuid .~ newValue
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & uuid .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & uuid .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity & uuid .~ newValue

instance HasUuid' EditQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> U.UUID
      get (EditListQuestionEvent' entity) = entity ^. uuid
      get (EditOptionsQuestionEvent' entity) = entity ^. uuid
      get (EditValueQuestionEvent' entity) = entity ^. uuid
      get (EditIntegrationQuestionEvent' entity) = entity ^. uuid
      set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & uuid .~ newValue
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & uuid .~ newValue
      set (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity & uuid .~ newValue
      set (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity & uuid .~ newValue

instance HasUuid' DeleteQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteQuestionEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteQuestionEvent -> U.UUID -> DeleteQuestionEvent
      set entity newValue = entity & uuid .~ newValue

instance HasParentUuid' AddQuestionEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> U.UUID
      get (AddListQuestionEvent' entity) = entity ^. parentUuid
      get (AddOptionsQuestionEvent' entity) = entity ^. parentUuid
      get (AddValueQuestionEvent' entity) = entity ^. parentUuid
      get (AddIntegrationQuestionEvent' entity) = entity ^. parentUuid
      set :: AddQuestionEvent -> U.UUID -> AddQuestionEvent
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & parentUuid .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue =
        AddIntegrationQuestionEvent' $ entity & parentUuid .~ newValue

instance HasParentUuid' EditQuestionEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> U.UUID
      get (EditListQuestionEvent' entity) = entity ^. parentUuid
      get (EditOptionsQuestionEvent' entity) = entity ^. parentUuid
      get (EditValueQuestionEvent' entity) = entity ^. parentUuid
      get (EditIntegrationQuestionEvent' entity) = entity ^. parentUuid
      set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & parentUuid .~ newValue
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & parentUuid .~ newValue
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

instance HasEntityUuid' AddQuestionEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddQuestionEvent -> U.UUID
      get (AddListQuestionEvent' entity) = entity ^. entityUuid
      get (AddOptionsQuestionEvent' entity) = entity ^. entityUuid
      get (AddValueQuestionEvent' entity) = entity ^. entityUuid
      get (AddIntegrationQuestionEvent' entity) = entity ^. entityUuid
      set :: AddQuestionEvent -> U.UUID -> AddQuestionEvent
      set (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity & entityUuid .~ newValue
      set (AddIntegrationQuestionEvent' entity) newValue =
        AddIntegrationQuestionEvent' $ entity & entityUuid .~ newValue

instance HasEntityUuid' EditQuestionEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditQuestionEvent -> U.UUID
      get (EditListQuestionEvent' entity) = entity ^. entityUuid
      get (EditOptionsQuestionEvent' entity) = entity ^. entityUuid
      get (EditValueQuestionEvent' entity) = entity ^. entityUuid
      get (EditIntegrationQuestionEvent' entity) = entity ^. entityUuid
      set :: EditQuestionEvent -> U.UUID -> EditQuestionEvent
      set (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity & entityUuid .~ newValue
      set (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity & entityUuid .~ newValue
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
