module Api.Handler.Typehint.TypehintHandler where

import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.Typehint.TypehintJM ()
import Api.Resource.Typehint.TypehintRequestJM ()
import Service.Typehint.TypehintService

postTypehintsA :: Endpoint
postTypehintsA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherDto <- runInAuthService $ getTypehints reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error
