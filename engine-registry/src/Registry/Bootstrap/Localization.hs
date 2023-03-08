module Registry.Bootstrap.Localization (
  loadLocalization,
) where

import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.Map.Strict as M
import Network.HTTP.Client (Response (..))
import Network.Wreq (get)

import Registry.Constant.Resource
import Registry.Util.Logger
import Shared.Model.Error.Error
import Shared.Service.File.FileService

loadLocalization serverConfig = do
  logInfo _CMP_LOCALIZATION "start loading localization"
  localLocalization <- loadLocalLocalization serverConfig
  remoteLocalization <- loadRemoteLocalization serverConfig
  logInfo _CMP_LOCALIZATION "localization loaded"
  return $ M.union remoteLocalization localLocalization

loadLocalLocalization serverConfig = do
  logInfo _CMP_LOCALIZATION "start loading local localization"
  eResult <- liftIO (loadJSONFile localizationFile :: IO (Either AppError (M.Map String String)))
  case eResult of
    Right result -> do
      logInfo _CMP_LOCALIZATION "local localization loaded"
      return result
    Left error -> do
      logError _CMP_LOCALIZATION "load failed"
      logError
        _CMP_LOCALIZATION
        ("can't load '" ++ localizationFile ++ "'. Maybe the file is missing or not well-formatted")
      logError _CMP_LOCALIZATION (show error)
      return M.empty

loadRemoteLocalization serverConfig = do
  logInfo _CMP_LOCALIZATION "start loading remote localization"
  let mUrl = serverConfig.general.remoteLocalizationUrl
  case mUrl of
    Just url -> do
      response <- liftIO $ get url
      case eitherDecode . responseBody $ response of
        Right body -> do
          logInfo _CMP_LOCALIZATION "remote localization loaded"
          return body
        Left error -> do
          logError _CMP_LOCALIZATION "server couln't deserialized response from localization server"
          return M.empty
    Nothing -> do
      logInfo _CMP_LOCALIZATION "there is no configured remote localization"
      return M.empty
