module Shared.Model.Context.AppContext where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)

data AppContext =
  AppContext
    { _appContextPool :: ConnectionPool
    , _appContextLocalization :: M.Map String String
    , _appContextTraceUuid :: U.UUID
    }
