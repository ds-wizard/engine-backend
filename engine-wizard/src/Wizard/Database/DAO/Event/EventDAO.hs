module Wizard.Database.DAO.Event.EventDAO where

import Control.Monad.Reader (asks)
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Model.Event.Event
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.BranchWithEvents ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid events = do
  appUuid <- asks _appContextAppUuid
  let sql = fromString "UPDATE branch SET events = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toJSONField events, toField appUuid, toField branchUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = do
  appUuid <- asks _appContextAppUuid
  let events = [] :: [Event]
  let sql = fromString "UPDATE branch SET events = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toJSONField events, toField appUuid, toField branchUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()
