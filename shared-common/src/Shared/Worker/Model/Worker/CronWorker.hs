module Shared.Worker.Model.Worker.CronWorker where

data CronWorker baseContext appContextM = CronWorker
  { name :: String
  , condition :: baseContext -> Bool
  , cronDefault :: String
  , cron :: baseContext -> String
  , function :: appContextM ()
  , wrapInTransaction :: Bool
  }
