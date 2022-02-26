module Wizard.Integration.Http.Config.Runner
  ( compileClientCss
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL

import LensesConfig
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Config.RequestMapper
import Wizard.Integration.Http.Config.ResponseMapper
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppHelper

compileClientCss :: App -> AppConfigLookAndFeel -> AppContextM BSL.ByteString
compileClientCss app lookAndFeel = do
  serverConfig <- asks _appContextServerConfig
  let styleBuilderUrl = serverConfig ^. general . clientStyleBuilderUrl
  clientUrl <- getAppClientUrl
  runRequest (toCompileClientCssRequest styleBuilderUrl clientUrl lookAndFeel) toCompileClientCssResponse
