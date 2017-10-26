module Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.Info.InfoDTO
import Context
import DSPConfig

getInfoA :: Context -> DSPConfig -> Scotty.ActionM ()
getInfoA context dspConfig =
  Scotty.json
    InfoDTO
    { _idtoName = dspConfig ^. dspcfgBuildInfo ^. biAppName
    , _idtoVersion = dspConfig ^. dspcfgBuildInfo ^. biAppVersion
    , _idtoBuiltAt = dspConfig ^. dspcfgBuildInfo ^. biBuiltAt
    }
