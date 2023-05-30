module Wizard.Bootstrap.Web (
  runWebServer,
  runApp,
  runMiddleware,
) where

import Control.Exception (SomeException)
import Control.Monad.Reader (runReaderT)
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
import Wizard.Api.Api
import Wizard.Api.Handler.Swagger.Api
import Wizard.Api.Middleware.LoggingMiddleware
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Util.Logger
import Wizard.Util.Sentry

runWebServer :: BaseContext -> IO ()
runWebServer context = do
  let config = context.serverConfig
  let webPort = config.general.serverPort
  let env = config.general.environment
  sentryHandler <- createSentryHandler context
  let settings = setPort webPort . setOnException sentryHandler $ defaultSettings
  runSettings settings (runMiddleware env $ runApp context)

-- --------------------------------
-- PRIVATE
-- --------------------------------
type ServerAPI =
  SwaggerAPI
    :<|> ApplicationAPI

serverApi :: Proxy ServerAPI
serverApi = Proxy

convert :: BaseContext -> BaseContextM a -> Handler a
convert baseContext function =
  let loggingLevel = baseContext.serverConfig.logging.level
   in Handler . runLogging loggingLevel $ runReaderT (runBaseContextM function) baseContext

appToServer :: BaseContext -> Server ServerAPI
appToServer baseContext = swaggerServer :<|> hoistServer applicationApi (convert baseContext) applicationServer

runApp :: BaseContext -> Application
runApp baseContext = serve serverApi (appToServer baseContext)

runMiddleware :: Environment -> Application -> Application
runMiddleware env =
  contentTypeMiddleware
    . corsMiddleware
    . errorMw @JSON @'["message", "status"]
    . loggingMiddleware env
    . optionsMiddleware

createSentryHandler :: BaseContext -> IO (Maybe WAI.Request -> SomeException -> IO ())
createSentryHandler context = do
  if context.serverConfig.sentry.enabled
    then do
      let sentryUrl = context.serverConfig.sentry.dsn
      sentryService <- createSentryService sentryUrl
      let buildVersion = context.buildInfoConfig.version
      return $ sentryOnException buildVersion sentryService
    else do
      return
        ( \_ error -> do
            print error
            return ()
        )
