module Wizard.Service.Config.Integration.IntegrationConfigService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeEither', decodeFileEither)
import System.Environment (lookupEnv)

import Shared.Common.Constant.Component
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.App.AppConfigService
import Wizard.Util.Logger

getFileIntegrationConfig :: String -> AppContextM (M.Map String String)
getFileIntegrationConfig sectionName = do
  serverConfig <- asks serverConfig
  mFileNameEnv <- liftIO $ lookupEnv "INTEGRATION_CONFIG_PATH"
  let integrationConfigPath =
        case mFileNameEnv of
          Just fileNameEnv -> fileNameEnv
          Nothing -> serverConfig.general.integrationConfig
  eIntConfig <- liftIO $ decodeFileEither integrationConfigPath
  case eIntConfig of
    Right intConfig -> return . fromMaybe M.empty . M.lookup sectionName $ intConfig
    Left error -> do
      logWarnU _CMP_SERVICE ("Failed to load file integration configuration (error: " ++ show error ++ ")")
      return M.empty

getAppIntegrationConfig :: String -> AppContextM (M.Map String String)
getAppIntegrationConfig sectionName = do
  appConfig <- getAppConfig
  let content = appConfig.knowledgeModel.integrationConfig
  let eIntConfig = decodeEither' . BS.pack $ content
  case eIntConfig of
    Right intConfig -> return . fromMaybe M.empty . M.lookup sectionName $ intConfig
    Left error -> do
      logWarnU _CMP_SERVICE ("Failed to load app integration configuration (error: " ++ show error ++ ")")
      return M.empty
