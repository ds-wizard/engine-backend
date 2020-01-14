module Registry.Specs.Common where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO, runReaderT)

import Registry.Model.Context.AppContext

fakeLogState :: String -> IO ()
fakeLogState _ = return ()

filterJustError :: LogSource -> LogLevel -> Bool
filterJustError _ LevelError = True
filterJustError _ _ = False

runInContext action appContext =
  runStdoutLoggingT . (filterLogger filterJustError) $ runReaderT (runAppContextM action) appContext

runInContextIO action appContext =
  liftIO . runStdoutLoggingT . (filterLogger filterJustError) $ runReaderT (runAppContextM action) appContext
