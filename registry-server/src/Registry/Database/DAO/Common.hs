module Registry.Database.DAO.Common (
  module Shared.Common.Database.DAO.Common,
  runInTransaction,
) where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger
import Shared.Common.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Common.Database.DAO.Common as S

runInTransaction :: AppContextM a -> AppContextM a
runInTransaction = S.runInTransaction logInfoU logWarnU
