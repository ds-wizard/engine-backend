module Wizard.Bootstrap.Web
  ( runWebServer
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Default (def)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import Web.Scotty.Trans (Options, scottyOptsT, settings, verbose)

import Shared.Model.Config.Environment
import Wizard.Api.Router
import Wizard.LensesConfig
import Wizard.Model.Context.BaseContext

runWebServer :: BaseContext -> IO ()
runWebServer context = do
  let o = getOptions context
  let r m = runStdoutLoggingT $ runReaderT (runBaseContextM m) context
  scottyOptsT o r (createEndpoints context)

-- --------------------------------
-- PRIVATE
-- --------------------------------
getOptions :: BaseContext -> Options
getOptions context =
  def
    { settings = getSettings context
    , verbose =
        case context ^. appConfig . general . environment of
          Production -> 0
          Staging -> 1
          Development -> 1
          Test -> 0
    }

getSettings :: BaseContext -> Settings
getSettings context =
  let webPort = context ^. appConfig . general . serverPort
   in setPort webPort defaultSettings
