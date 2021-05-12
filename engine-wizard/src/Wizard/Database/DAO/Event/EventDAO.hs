module Wizard.Database.DAO.Event.EventDAO where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Model.Event.Event
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.BranchWithEvents ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid events = do
  let params = [toJSONField events, toField branchUuid]
  let action conn = execute conn "UPDATE branch SET events = ? WHERE uuid = ?" params
  runDB action
  return ()

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = do
  let events = [] :: [Event]
  let params = [toJSONField events, toField branchUuid]
  let action conn = execute conn "UPDATE branch SET events = ? WHERE uuid = ?" params
  runDB action
  return ()
