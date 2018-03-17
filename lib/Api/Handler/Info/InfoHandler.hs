module Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.Info.InfoDTO
import Common.Context
import Common.DSWConfig

getInfoA :: Context -> DSWConfig -> Scotty.ActionM ()
getInfoA context dswConfig =
  sendJson
    InfoDTO
    { _idtoName = dswConfig ^. dswcfgBuildInfo ^. biAppName
    , _idtoVersion = dswConfig ^. dswcfgBuildInfo ^. biAppVersion
    , _idtoBuiltAt = dswConfig ^. dswcfgBuildInfo ^. biBuiltAt
    }
