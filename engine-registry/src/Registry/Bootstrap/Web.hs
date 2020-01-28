module Registry.Bootstrap.Web
  ( runWebServer
  , app
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
import Registry.Api.Middleware.CORSMiddleware
import Registry.Api.Middleware.ErrorMiddleware
import Registry.Api.Middleware.LoggingMiddleware
import Registry.Model.Context.BaseContext

runWebServer :: BaseContext -> IO ()
runWebServer context = do
  let config = context ^. appConfig
  let webPort = config ^. general . serverPort
  let env = config ^. general . environment
  run
    webPort
    (errorMw @JSON @'[ "message", "status"] . errorMiddleware . corsMiddleware . loggingMiddleware env $ app context)

-- --------------------------------
-- PRIVATE
-- --------------------------------
convert :: BaseContext -> BaseContextM a -> Handler a
convert baseContext function = Handler . runStdoutLoggingT $ runReaderT (runBaseContextM function) baseContext

appToServer :: BaseContext -> Server AppAPI
appToServer baseContext = hoistServer appApi (convert baseContext) appServer

app :: BaseContext -> Application
app baseContext = serve appApi (appToServer baseContext)
