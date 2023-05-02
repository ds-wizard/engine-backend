module SharedTest.Specs.Common where

import Control.Monad.Logger

fakeLogState :: String -> IO ()
fakeLogState _ = return ()

filterJustError :: LogSource -> LogLevel -> Bool
filterJustError _ LevelError = True
filterJustError _ _ = False
