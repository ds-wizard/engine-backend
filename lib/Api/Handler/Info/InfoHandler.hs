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
  dswConfig <- lift $ asks _baseContextConfig
  json
    InfoDTO
    { _idtoName = dswConfig ^. buildInfo ^. appName
    , _idtoVersion = dswConfig ^. buildInfo ^. appVersion
    , _idtoBuiltAt = dswConfig ^. buildInfo ^. builtAt
    }
