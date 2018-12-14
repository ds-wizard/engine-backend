module Api.Handler.Token.TokenHandler where

import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (json, status)

import Api.Handler.Common
import Service.Token.TokenService

postTokenA :: Endpoint
postTokenA =
  getReqDto $ \reqDto -> do
    eitherTokenDto <- runInUnauthService $ getToken reqDto
    case eitherTokenDto of
      Right tokenDto -> do
        status created201
        json tokenDto
      Left error -> sendError error
