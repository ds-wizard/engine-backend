module Wizard.Database.DAO.Limit.AppLimitDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Limit.AppLimit ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Limit.AppLimit

entityName = "app_limit"

findAppLimits :: AppContextM [AppLimit]
findAppLimits = createFindEntitiesFn entityName

findAppLimitById :: String -> AppContextM AppLimit
findAppLimitById uuid = createFindEntityByFn entityName [("uuid", uuid)]

findCurrentAppLimit :: AppContextM AppLimit
findCurrentAppLimit = do
  appUuid <- asks _appContextAppUuid
  findAppLimitById (U.toString appUuid)

insertAppLimit :: AppLimit -> AppContextM Int64
insertAppLimit = createInsertFn entityName

deleteAppLimits :: AppContextM Int64
deleteAppLimits = createDeleteEntitiesFn entityName
