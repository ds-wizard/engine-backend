module Api.Middleware.LoggingMiddleware where

import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)

import Model.Config.Environment

loggingMiddleware :: Environment -> Middleware
loggingMiddleware Production = logStdout
loggingMiddleware Staging = logStdoutDev
loggingMiddleware Development = logStdoutDev
loggingMiddleware Test = id
