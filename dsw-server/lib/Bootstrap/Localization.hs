module Bootstrap.Localization where

import Control.Lens ((^.), (^?))
import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Lens (_Value)
import qualified Data.Map.Strict as M
import Network.Wreq (get, responseBody)

import Constant.Component
import LensesConfig hiding (headers)
import Util.Logger (logError, logInfo, msg)

loadLocalization appConfig = do
  logInfo $ msg _CMP_LOCALIZATION "start loading localization"
  let mUrl = appConfig ^. general . localizationUrl
  case mUrl of
    Just url -> do
      response <- liftIO $ get url
      case response ^? responseBody . _Value of
        Just body ->
          case eitherDecode $ response ^. responseBody of
            Right body -> do
              logInfo $ msg _CMP_LOCALIZATION "localization loaded"
              return body
            Left error -> do
              logError $ msg _CMP_LOCALIZATION "server couln't deserialized response from localization server"
              return M.empty
        Nothing -> do
          logError $ msg _CMP_LOCALIZATION "server couln't retrieved response from localization server"
          return M.empty
    Nothing -> do
      logInfo $ msg _CMP_LOCALIZATION "there is no configured localization"
      return M.empty
