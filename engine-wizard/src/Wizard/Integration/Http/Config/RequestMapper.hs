module Wizard.Integration.Http.Config.RequestMapper
  ( toCompileClientCssRequest
  ) where

import Control.Lens ((^.))
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import Prelude hiding (lookup)

import LensesConfig
import Wizard.Integration.Resource.Config.CompileClientCssIDTO
import Wizard.Integration.Resource.Config.CompileClientCssIJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Http.HttpRequest

toCompileClientCssRequest :: String -> String -> AppConfigLookAndFeel -> HttpRequest
toCompileClientCssRequest reqUrl clientUrl lookAndFeel =
  HttpRequest
    { _httpRequestRequestMethod = "POST"
    , _httpRequestRequestUrl = reqUrl ++ "/simple"
    , _httpRequestRequestHeaders = M.fromList [("Content-Type", "application/json")]
    , _httpRequestRequestBody =
        BSL.toStrict . encode $
        CompileClientCssIDTO
          { _compileClientCssIDTOClientUrl = clientUrl
          , _compileClientCssIDTOLogoUrl = lookAndFeel ^. logoUrl
          , _compileClientCssIDTOPrimaryColor = lookAndFeel ^. primaryColor
          , _compileClientCssIDTOIllustrationsColor = lookAndFeel ^. illustrationsColor
          }
    , _httpRequestMultipartFileName = Nothing
    }
