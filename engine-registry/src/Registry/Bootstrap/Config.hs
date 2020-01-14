module Registry.Bootstrap.Config where

import Control.Monad.Reader (liftIO)

import Registry.Constant.Component
import Registry.Util.Logger

hLoadConfig fileName loadFn callback = do
  eitherConfig <- liftIO (loadFn fileName)
  case eitherConfig of
    Right config -> do
      logInfo $ msg _CMP_CONFIG ("'" ++ fileName ++ "' loaded")
      callback config
    Left error -> do
      logError $ msg _CMP_CONFIG "load failed"
      logError $ msg _CMP_CONFIG ("can't load '" ++ fileName ++ "'. Maybe the file is missing or not well-formatted")
      logError $ msg _CMP_CONFIG (show error)
