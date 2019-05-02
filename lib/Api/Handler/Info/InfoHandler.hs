module Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Info.InfoDTO
import LensesConfig
import Model.Context.BaseContext

getInfoA :: Endpoint
getInfoA = do
  buildInfoConfig <- lift $ asks _baseContextBuildInfoConfig
  json
    InfoDTO
    { _idtoName = buildInfoConfig ^. name
    , _idtoVersion = buildInfoConfig ^. version
    , _idtoBuiltAt = buildInfoConfig ^. builtAt
    }
