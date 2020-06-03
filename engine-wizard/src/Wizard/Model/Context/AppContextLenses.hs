module Wizard.Model.Context.AppContextLenses where

import Control.Lens ((&), (.~), (^.))
import Database.Persist.MongoDB (ConnectionPool)

import LensesConfig
import Shared.Model.Context.AppContextLenses
import Wizard.Model.Context.AppContext

instance HasPool' AppContext where
  pool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> ConnectionPool
      get entity = entity ^. pool
      set :: AppContext -> ConnectionPool -> AppContext
      set entity newValue = entity & pool .~ newValue
