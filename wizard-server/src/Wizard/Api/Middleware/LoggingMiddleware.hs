module Wizard.Api.Middleware.LoggingMiddleware where

import qualified Data.Text as T
import Network.Wai (Middleware)

import Shared.Common.Api.Middleware.LoggingMiddleware
import Shared.Common.Model.Config.Environment
import Shared.Common.Util.Token
import WizardLib.Public.Service.UserToken.UserTokenUtil

loggingMiddleware :: Environment -> Middleware
loggingMiddleware = createLoggingMiddleware extractIdentity

extractIdentity :: T.Text -> Maybe String
extractIdentity tokenHeader = separateToken (T.unpack tokenHeader) >>= getUserUuidFromToken
