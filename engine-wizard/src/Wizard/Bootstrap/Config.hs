module Wizard.Bootstrap.Config where

import Control.Monad.Reader (liftIO)

import Wizard.Constant.Component
import Wizard.Util.Logger

hLoadConfig fileName loadFn callback = do
  eitherConfig <- liftIO (loadFn fileName)
  case eitherConfig of
    Right config -> do
      logInfo _CMP_CONFIG ("'" ++ fileName ++ "' loaded")
      callback config
    Left error -> do
      logError _CMP_CONFIG "load failed"
      logError _CMP_CONFIG ("can't load '" ++ fileName ++ "'. Maybe the file is missing or not well-formatted")
      logError _CMP_CONFIG (show error)
