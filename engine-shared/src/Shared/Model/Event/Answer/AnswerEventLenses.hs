module Shared.Model.Event.Answer.AnswerEventLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Answer.AnswerEvent

instance HasUuid' AddAnswerEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddAnswerEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddAnswerEvent -> U.UUID -> AddAnswerEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditAnswerEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditAnswerEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditAnswerEvent -> U.UUID -> EditAnswerEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteAnswerEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteAnswerEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteAnswerEvent -> U.UUID -> DeleteAnswerEvent
      set entity newValue = entity & uuid .~ newValue

instance HasParentUuid' AddAnswerEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddAnswerEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddAnswerEvent -> U.UUID -> AddAnswerEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditAnswerEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditAnswerEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditAnswerEvent -> U.UUID -> EditAnswerEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteAnswerEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteAnswerEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteAnswerEvent -> U.UUID -> DeleteAnswerEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasEntityUuid' AddAnswerEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddAnswerEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddAnswerEvent -> U.UUID -> AddAnswerEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditAnswerEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditAnswerEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditAnswerEvent -> U.UUID -> EditAnswerEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteAnswerEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteAnswerEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteAnswerEvent -> U.UUID -> DeleteAnswerEvent
      set entity newValue = entity & entityUuid .~ newValue
