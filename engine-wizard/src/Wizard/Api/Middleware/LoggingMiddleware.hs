module Wizard.Api.Middleware.LoggingMiddleware where

import qualified Data.Text as T
import Network.Wai (Middleware)

import Shared.Api.Middleware.LoggingMiddleware
import Shared.Model.Config.Environment
import Shared.Util.Token
import Wizard.Service.UserToken.UserTokenUtil

loggingMiddleware :: Environment -> Middleware
loggingMiddleware = createLoggingMiddleware extractIdentity

extractIdentity :: T.Text -> Maybe String
extractIdentity tokenHeader = separateToken (T.unpack tokenHeader) >>= getUserUuidFromToken
