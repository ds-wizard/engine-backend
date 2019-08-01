module Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Info.InfoDTO
import Api.Resource.Info.InfoJM ()
import LensesConfig
import Model.Context.BaseContext

getInfoA :: Endpoint
getInfoA = do
  buildInfoConfig <- lift $ asks _baseContextBuildInfoConfig
  json
    InfoDTO
    { _infoDTOName = buildInfoConfig ^. name
    , _infoDTOVersion = buildInfoConfig ^. version
    , _infoDTOBuiltAt = buildInfoConfig ^. builtAt
    }
