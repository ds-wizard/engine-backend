module Shared.Model.Event.Move.MoveEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Move.MoveEvent

instance HasUuid' MoveQuestionEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveQuestionEvent -> U.UUID
      get entity = entity ^. uuid
      set :: MoveQuestionEvent -> U.UUID -> MoveQuestionEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' MoveAnswerEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveAnswerEvent -> U.UUID
      get entity = entity ^. uuid
      set :: MoveAnswerEvent -> U.UUID -> MoveAnswerEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' MoveChoiceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveChoiceEvent -> U.UUID
      get entity = entity ^. uuid
      set :: MoveChoiceEvent -> U.UUID -> MoveChoiceEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' MoveExpertEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveExpertEvent -> U.UUID
      get entity = entity ^. uuid
      set :: MoveExpertEvent -> U.UUID -> MoveExpertEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' MoveReferenceEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveReferenceEvent -> U.UUID
      get entity = entity ^. uuid
      set :: MoveReferenceEvent -> U.UUID -> MoveReferenceEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' MoveQuestionEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveQuestionEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: MoveQuestionEvent -> U.UUID -> MoveQuestionEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' MoveAnswerEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveAnswerEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: MoveAnswerEvent -> U.UUID -> MoveAnswerEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' MoveChoiceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveChoiceEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: MoveChoiceEvent -> U.UUID -> MoveChoiceEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' MoveExpertEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveExpertEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: MoveExpertEvent -> U.UUID -> MoveExpertEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' MoveReferenceEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveReferenceEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: MoveReferenceEvent -> U.UUID -> MoveReferenceEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' MoveQuestionEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveQuestionEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: MoveQuestionEvent -> U.UUID -> MoveQuestionEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' MoveAnswerEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveAnswerEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: MoveAnswerEvent -> U.UUID -> MoveAnswerEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' MoveChoiceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveChoiceEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: MoveChoiceEvent -> U.UUID -> MoveChoiceEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' MoveExpertEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveExpertEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: MoveExpertEvent -> U.UUID -> MoveExpertEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' MoveReferenceEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveReferenceEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: MoveReferenceEvent -> U.UUID -> MoveReferenceEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' MoveQuestionEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveQuestionEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: MoveQuestionEvent -> UTCTime -> MoveQuestionEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' MoveAnswerEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveAnswerEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: MoveAnswerEvent -> UTCTime -> MoveAnswerEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' MoveChoiceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveChoiceEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: MoveChoiceEvent -> UTCTime -> MoveChoiceEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' MoveExpertEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveExpertEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: MoveExpertEvent -> UTCTime -> MoveExpertEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' MoveReferenceEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: MoveReferenceEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: MoveReferenceEvent -> UTCTime -> MoveReferenceEvent
      set entity newValue = entity & createdAt .~ newValue
