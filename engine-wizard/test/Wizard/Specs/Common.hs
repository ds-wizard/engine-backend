module Wizard.Specs.Common where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO, runReaderT)

import SharedTest.Specs.Common
import Wizard.Model.Context.AppContext

runInContext action appContext =
  runStdoutLoggingT . (filterLogger filterJustError) $ runReaderT (runAppContextM action) appContext

runInContextIO action appContext =
  liftIO . runStdoutLoggingT . (filterLogger filterJustError) $ runReaderT (runAppContextM action) appContext
