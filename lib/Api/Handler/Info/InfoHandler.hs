module Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.Info.InfoDTO
import Common.Context
import LensesConfig
import Model.Config.DSWConfig

getInfoA :: Context -> DSWConfig -> Scotty.ActionM ()
getInfoA context dswConfig =
  sendJson
    InfoDTO
    { _idtoName = dswConfig ^. buildInfo ^. appName
    , _idtoVersion = dswConfig ^. buildInfo ^. appVersion
    , _idtoBuiltAt = dswConfig ^. buildInfo ^. builtAt
    }
