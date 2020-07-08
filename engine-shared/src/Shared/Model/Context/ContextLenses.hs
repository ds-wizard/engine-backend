module Shared.Model.Context.ContextLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)

import LensesConfig
import Shared.Model.Context.AppContext
import Shared.Model.Context.BaseContext

class HasPool' entity where
  pool' :: Functor f => (ConnectionPool -> f ConnectionPool) -> entity -> f entity

instance HasPool' AppContext where
  pool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> ConnectionPool
      get entity = entity ^. pool
      set :: AppContext -> ConnectionPool -> AppContext
      set entity newValue = entity & pool .~ newValue

instance HasPool' BaseContext where
  pool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> ConnectionPool
      get entity = entity ^. pool
      set :: BaseContext -> ConnectionPool -> BaseContext
      set entity newValue = entity & pool .~ newValue

class HasLocalization' entity where
  localization' :: Functor f => (M.Map String String -> f (M.Map String String)) -> entity -> f entity

instance HasLocalization' AppContext where
  localization' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> M.Map String String
      get entity = entity ^. localization
      set :: AppContext -> M.Map String String -> AppContext
      set entity newValue = entity & localization .~ newValue

instance HasLocalization' BaseContext where
  localization' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> M.Map String String
      get entity = entity ^. localization
      set :: BaseContext -> M.Map String String -> BaseContext
      set entity newValue = entity & localization .~ newValue

class HasTraceUuid' entity where
  traceUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

instance HasTraceUuid' AppContext where
  traceUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> U.UUID
      get entity = entity ^. traceUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity newValue = entity & traceUuid .~ newValue
