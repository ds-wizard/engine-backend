module Registry.Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Registry.Api.Handler.Common
import Registry.Api.Resource.Info.InfoDTO
import Registry.Api.Resource.Info.InfoJM ()
import Registry.LensesConfig
import Registry.Model.Context.BaseContext

getInfoA :: Endpoint
getInfoA = do
  buildInfoConfig <- lift $ asks _baseContextBuildInfoConfig
  json
    InfoDTO
      { _infoDTOName = buildInfoConfig ^. name
      , _infoDTOVersion = buildInfoConfig ^. version
      , _infoDTOBuiltAt = buildInfoConfig ^. builtAt
      }
