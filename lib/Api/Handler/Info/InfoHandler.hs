module Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.Info.InfoDTO
import Common.Context
import Common.DSPConfig

getInfoA :: Context -> DSPConfig -> Scotty.ActionM ()
getInfoA context dspConfig =
  sendJson
    InfoDTO
    { _idtoName = dspConfig ^. dspcfgBuildInfo ^. biAppName
    , _idtoVersion = dspConfig ^. dspcfgBuildInfo ^. biAppVersion
    , _idtoBuiltAt = dspConfig ^. dspcfgBuildInfo ^. biBuiltAt
    }
