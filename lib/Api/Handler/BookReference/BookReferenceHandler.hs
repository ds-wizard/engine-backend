module Api.Handler.BookReference.BookReferenceHandler where

import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.BookReference.BookReferenceJM ()
import Service.BookReference.BookReferenceService

getBookReferenceA :: Endpoint
getBookReferenceA = do
  brShortUuid <- param "brShortUuid"
  eitherDto <- runInUnauthService $ getBookReference brShortUuid
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error
