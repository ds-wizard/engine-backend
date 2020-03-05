module Wizard.Specs.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Either
import Test.Hspec

import Wizard.Model.Context.AppContext

fakeLogState :: String -> IO ()
fakeLogState _ = return ()

filterJustError :: LogSource -> LogLevel -> Bool
filterJustError _ LevelError = True
filterJustError _ _ = False

runInContext action appContext =
  runExceptT . runStdoutLoggingT . filterLogger filterJustError $ runReaderT (runAppContextM action) appContext

runInContextIO action appContext =
  liftIO . runExceptT $ runStdoutLoggingT . filterLogger filterJustError $ runReaderT (runAppContextM action) appContext

shouldSucceed appContext fn = do
  result <- runInContext fn appContext
  isRight result `shouldBe` True

shouldFailed appContext fn = do
  result <- runInContext fn appContext
  isRight result `shouldBe` False
