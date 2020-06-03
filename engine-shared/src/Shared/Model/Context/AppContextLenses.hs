module Shared.Model.Context.AppContextLenses where

import Control.Lens ((&), (.~), (^.))
import Database.Persist.MongoDB (ConnectionPool)

import LensesConfig
import Shared.Model.Context.AppContext

class HasPool' entity where
  pool' :: Functor f => (ConnectionPool -> f ConnectionPool) -> entity -> f entity

instance HasPool' AppContext where
  pool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> ConnectionPool
      get entity = entity ^. pool
      set :: AppContext -> ConnectionPool -> AppContext
      set entity newValue = entity & pool .~ newValue
