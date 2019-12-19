module Wizard.Api.Handler.BookReference.BookReferenceHandler where

import Web.Scotty.Trans (json, param)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.BookReference.BookReferenceJM ()
import Wizard.Service.BookReference.BookReferenceService

getBookReferenceA :: Endpoint
getBookReferenceA = do
  brShortUuid <- param "brShortUuid"
  eitherDto <- runInUnauthService $ getBookReference brShortUuid
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error
