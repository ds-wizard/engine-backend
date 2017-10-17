module Context where

import Control.Lens (makeLenses)
import Database.Persist.MongoDB (ConnectionPool)

data Config =
  Config

data Context = Context
  { _ctxDbPool :: ConnectionPool
  , _ctxConfig :: Config
  }

makeLenses ''Config  
makeLenses ''Context  