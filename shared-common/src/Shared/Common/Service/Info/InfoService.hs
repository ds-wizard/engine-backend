module Shared.Common.Service.Info.InfoService where

import Control.Monad.Reader (asks)

import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Context.AppContext
import Shared.Component.Database.DAO.Component.ComponentDAO

getInfo :: AppContextC s sc m => m InfoDTO
getInfo = do
  buildInfoConfig <- asks (.buildInfoConfig')
  components <- findComponents
  return
    InfoDTO
      { name = buildInfoConfig.name
      , version = buildInfoConfig.version
      , builtAt = buildInfoConfig.builtAt
      , components = components
      }
