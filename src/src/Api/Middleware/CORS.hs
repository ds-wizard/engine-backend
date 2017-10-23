module Api.Middleware.CORS where

import Network.Wai.Middleware.Cors

corsMiddleware = cors (const $ Just simpleCorsResourcePolicy)
