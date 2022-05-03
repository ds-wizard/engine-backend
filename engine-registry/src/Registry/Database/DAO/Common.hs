module Registry.Database.DAO.Common
  ( module Shared.Database.DAO.Common
  , runInTransaction
  ) where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger
import qualified Shared.Database.DAO.Common as S
import Shared.Database.DAO.Common hiding (runInTransaction)

runInTransaction :: AppContextM a -> AppContextM a
runInTransaction = S.runInTransaction logInfoU logErrorU
