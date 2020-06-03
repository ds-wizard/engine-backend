module Shared.Model.Context.AppContext where

import Database.Persist.MongoDB (ConnectionPool)

data AppContext =
  AppContext
    { _appContextPool :: ConnectionPool
    }
