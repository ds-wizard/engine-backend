module Shared.Model.Event.Chapter.ChapterEventLenses where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Chapter.ChapterEvent

instance HasUuid' AddChapterEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChapterEvent -> U.UUID
      get entity = entity ^. uuid
      set :: AddChapterEvent -> U.UUID -> AddChapterEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' EditChapterEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChapterEvent -> U.UUID
      get entity = entity ^. uuid
      set :: EditChapterEvent -> U.UUID -> EditChapterEvent
      set entity newValue = entity & uuid .~ newValue

instance HasUuid' DeleteChapterEvent where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChapterEvent -> U.UUID
      get entity = entity ^. uuid
      set :: DeleteChapterEvent -> U.UUID -> DeleteChapterEvent
      set entity newValue = entity & uuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddChapterEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChapterEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: AddChapterEvent -> U.UUID -> AddChapterEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' EditChapterEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChapterEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: EditChapterEvent -> U.UUID -> EditChapterEvent
      set entity newValue = entity & parentUuid .~ newValue

instance HasParentUuid' DeleteChapterEvent where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChapterEvent -> U.UUID
      get entity = entity ^. parentUuid
      set :: DeleteChapterEvent -> U.UUID -> DeleteChapterEvent
      set entity newValue = entity & parentUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddChapterEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChapterEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: AddChapterEvent -> U.UUID -> AddChapterEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' EditChapterEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChapterEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: EditChapterEvent -> U.UUID -> EditChapterEvent
      set entity newValue = entity & entityUuid .~ newValue

instance HasEntityUuid' DeleteChapterEvent where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChapterEvent -> U.UUID
      get entity = entity ^. entityUuid
      set :: DeleteChapterEvent -> U.UUID -> DeleteChapterEvent
      set entity newValue = entity & entityUuid .~ newValue

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddChapterEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AddChapterEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: AddChapterEvent -> UTCTime -> AddChapterEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' EditChapterEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: EditChapterEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: EditChapterEvent -> UTCTime -> EditChapterEvent
      set entity newValue = entity & createdAt .~ newValue

instance HasCreatedAt' DeleteChapterEvent where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: DeleteChapterEvent -> UTCTime
      get entity = entity ^. createdAt
      set :: DeleteChapterEvent -> UTCTime -> DeleteChapterEvent
      set entity newValue = entity & createdAt .~ newValue
