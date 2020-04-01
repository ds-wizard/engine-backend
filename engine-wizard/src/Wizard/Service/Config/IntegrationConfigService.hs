module Wizard.Service.Config.IntegrationConfigService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)

import LensesConfig hiding (items)
import Wizard.Constant.Component
import Wizard.Model.Context.AppContext
import Wizard.Util.Logger (logWarnU, msg)

getIntegrationConfig :: String -> AppContextM (M.Map String String)
getIntegrationConfig sectionName = do
  serverConfig <- asks _appContextServerConfig
  let integrationConfigPath = serverConfig ^. general . integrationConfig
  eIntConfig <- liftIO $ decodeFileEither integrationConfigPath
  case eIntConfig of
    Right intConfig -> return . fromMaybe M.empty . M.lookup sectionName $ intConfig
    Left error -> do
      logWarnU $ msg _CMP_SERVICE ("Failed to load integration configuration (error: " ++ show error ++ ")")
      return M.empty
