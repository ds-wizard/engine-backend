module Wizard.Bootstrap.Web
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
import Shared.Api.Middleware.CORSMiddleware
import Shared.Api.Middleware.OptionsMiddleware
import Shared.Model.Config.Environment
import Wizard.Api.Api
import Wizard.Api.Middleware.LoggingMiddleware
import Wizard.Model.Context.BaseContext

runWebServer :: BaseContext -> IO ()
runWebServer context = do
  let config = context ^. serverConfig
  let webPort = config ^. general . serverPort
  let env = config ^. general . environment
  run webPort (runMiddleware env $ runApp context)

-- --------------------------------
-- PRIVATE
-- --------------------------------
convert :: BaseContext -> BaseContextM a -> Handler a
convert baseContext function = Handler . runStdoutLoggingT $ runReaderT (runBaseContextM function) baseContext

appToServer :: BaseContext -> Server AppAPI
appToServer baseContext = hoistServer appApi (convert baseContext) appServer

runApp :: BaseContext -> Application
runApp baseContext = serve appApi (appToServer baseContext)

runMiddleware :: Environment -> Application -> Application
runMiddleware env = corsMiddleware . errorMw @JSON @'[ "message", "status"] . loggingMiddleware env . optionsMiddleware
