module Api.Handler.BookReference.BookReferenceHandler where

import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Service.BookReference.BookReferenceService

getBookReferenceA :: Endpoint
getBookReferenceA = do
  brShortUuid <- param "brShortUuid"
  eitherDto <- lift $ getBookReference brShortUuid
  case eitherDto of
    Right dto -> json dto
    Left error -> sendError error
