module Wizard.Database.DAO.ActionKey.ActionKeyDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.ActionKey.ActionKey ()
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "action_key"

findActionKeys :: AppContextM [ActionKey]
findActionKeys = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findActionKeyByHash :: String -> AppContextM ActionKey
findActionKeyByHash hash = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]

insertActionKey :: ActionKey -> AppContextM Int64
insertActionKey = createInsertFn entityName

deleteActionKeys :: AppContextM Int64
deleteActionKeys = createDeleteEntitiesFn entityName

deleteActionKeyByHash :: String -> AppContextM Int64
deleteActionKeyByHash hash = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("hash", hash)]
