module Wizard.Integration.Http.Config.RequestMapper (
  toCompileClientCssRequest,
) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Common.Model.Http.HttpRequest
import Wizard.Integration.Resource.Config.CompileClientCssIDTO
import Wizard.Integration.Resource.Config.CompileClientCssIJM ()
import Wizard.Model.Config.AppConfig

toCompileClientCssRequest :: String -> String -> AppConfigLookAndFeel -> HttpRequest
toCompileClientCssRequest reqUrl clientUrl lookAndFeel =
  HttpRequest
    { requestMethod = "POST"
    , requestUrl = reqUrl ++ "/simple"
    , requestHeaders = M.fromList [("Content-Type", "application/json")]
    , requestBody =
        BSL.toStrict . encode $
          CompileClientCssIDTO
            { clientUrl = clientUrl
            , logoUrl = lookAndFeel.logoUrl
            , primaryColor = lookAndFeel.primaryColor
            , illustrationsColor = lookAndFeel.illustrationsColor
            }
    , multipart = Nothing
    }
