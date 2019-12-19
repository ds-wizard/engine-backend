module Wizard.Api.Handler.Info.InfoHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Info.InfoDTO
import Wizard.Api.Resource.Info.InfoJM ()
import Wizard.LensesConfig
import Wizard.Model.Context.BaseContext

getInfoA :: Endpoint
getInfoA = do
  buildInfoConfig <- lift $ asks _baseContextBuildInfoConfig
  json
    InfoDTO
      { _infoDTOName = buildInfoConfig ^. name
      , _infoDTOVersion = buildInfoConfig ^. version
      , _infoDTOBuiltAt = buildInfoConfig ^. builtAt
      }
