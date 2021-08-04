module Wizard.Database.DAO.Event.EventDAO where

import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Model.Event.Event
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.BranchWithEvents ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid events = do
  let params = [toJSONField events, toField branchUuid]
  let sql = "UPDATE branch SET events = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action
  return ()

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = do
  let events = [] :: [Event]
  let params = [toJSONField events, toField branchUuid]
  let sql = "UPDATE branch SET events = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action
  return ()
