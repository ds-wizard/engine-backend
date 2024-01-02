module Shared.Common.Bootstrap.Web where

import Control.Exception (SomeException)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (Value)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant

import Shared.Common.Api.Middleware.CORSMiddleware
import Shared.Common.Api.Middleware.ContentTypeMiddleware
import Shared.Common.Api.Middleware.OptionsMiddleware
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.BaseContext
import Shared.Common.Util.Logger
import Shared.Common.Util.Sentry

runWebServerFactory
  :: (BaseContextType context sc, HasServer api '[])
  => context
  -> (Maybe String -> [(String, Value)])
  -> (Environment -> WAI.Middleware)
  -> Proxy api
  -> (context -> Server api)
  -> IO ()
runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer = do
  let webPort = context.serverConfig'.serverPort'
  let env = context.serverConfig'.environment'
  sentryHandler <- createSentryHandler context getSentryIdentity
  let settings = setPort webPort . setOnException sentryHandler $ defaultSettings
  runSettings settings (runMiddleware env loggingMiddleware $ serve webApi (webServer context))

convert
  :: BaseContextType context sc
  => context
  -> (function -> ReaderT context (LoggingT (ExceptT ServerError IO)) a)
  -> function
  -> Handler a
convert baseContext runBaseContextM function =
  let loggingLevel = baseContext.serverConfig'.logging.level
   in Handler . runLogging loggingLevel $ runReaderT (runBaseContextM function) baseContext

runMiddleware :: Environment -> (Environment -> WAI.Middleware) -> Application -> Application
runMiddleware env loggingMiddleware =
  contentTypeMiddleware
    . corsMiddleware
    . errorMw @JSON @'["message", "status"]
    . loggingMiddleware env
    . optionsMiddleware

createSentryHandler :: BaseContextType context sc => context -> (Maybe String -> [(String, Value)]) -> IO (Maybe WAI.Request -> SomeException -> IO ())
createSentryHandler context getSentryIdentity = do
  if context.serverConfig'.sentry'.enabled
    then do
      let sentryUrl = context.serverConfig'.sentry'.dsn
      sentryService <- createSentryService sentryUrl
      let buildVersion = context.buildInfoConfig'.version
      return $ sentryOnException buildVersion getSentryIdentity sentryService
    else do
      return
        ( \_ error -> do
            print error
            return ()
        )
