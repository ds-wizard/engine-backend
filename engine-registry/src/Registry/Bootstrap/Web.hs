module Registry.Bootstrap.Web
  ( runWebServer
  , runApp
  , runMiddleware
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant

import LensesConfig
import Registry.Api.Api
import Registry.Api.Handler.Swagger.Api
import Registry.Api.Middleware.LoggingMiddleware
import Registry.Model.Context.BaseContext
import Shared.Api.Middleware.CORSMiddleware
import Shared.Api.Middleware.OptionsMiddleware
import Shared.Model.Config.Environment

runWebServer :: BaseContext -> IO ()
runWebServer context = do
  let config = context ^. serverConfig
  let webPort = config ^. general . serverPort
  let env = config ^. general . environment
  run webPort (runMiddleware env $ runApp context)

-- --------------------------------
-- PRIVATE
-- --------------------------------
type ServerAPI
   = SwaggerAPI
     :<|> AppAPI

serverApi :: Proxy ServerAPI
serverApi = Proxy

convert :: BaseContext -> BaseContextM a -> Handler a
convert baseContext function = Handler . runStdoutLoggingT $ runReaderT (runBaseContextM function) baseContext

appToServer :: BaseContext -> Server ServerAPI
appToServer baseContext = swaggerServer :<|> (hoistServer appApi (convert baseContext) appServer)

runApp :: BaseContext -> Application
runApp baseContext = serve serverApi (appToServer baseContext)

runMiddleware :: Environment -> Application -> Application
runMiddleware env = corsMiddleware . errorMw @JSON @'[ "message", "status"] . loggingMiddleware env . optionsMiddleware
