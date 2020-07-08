module Shared.Model.Context.BaseContext where

import qualified Data.Map.Strict as M
import Database.Persist.MongoDB (ConnectionPool)

data BaseContext =
  BaseContext
    { _baseContextPool :: ConnectionPool
    , _baseContextLocalization :: M.Map String String
    }
