module Registry.Database.DAO.Common (
  module Shared.Common.Database.DAO.Common,
  runInTransaction,
) where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Common.Database.DAO.Common as S
import Shared.Common.Util.Logger

runInTransaction :: AppContextM a -> AppContextM a
runInTransaction = S.runInTransaction logInfoI logWarnI
