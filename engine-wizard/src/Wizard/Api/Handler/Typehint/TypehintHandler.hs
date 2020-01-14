module Wizard.Api.Handler.Typehint.TypehintHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Typehint.TypehintJM ()
import Wizard.Api.Resource.Typehint.TypehintRequestJM ()
import Wizard.Service.Typehint.TypehintService

postTypehintsA :: Endpoint
postTypehintsA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherDto <- runInAuthService $ getTypehints reqDto
      case eitherDto of
        Right dto -> json dto
        Left error -> sendError error
