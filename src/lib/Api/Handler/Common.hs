module Api.Handler.Common where

import Control.Monad.Reader
import Data.Aeson
import Data.ByteString
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (hContentType, notFound404)
import Network.HTTP.Types.Method (methodOptions)
import Network.HTTP.Types.Status
       (ok200, badRequest400, unauthorized401, forbidden403, notFound404,
        internalServerError500)
import Network.Wai
import qualified Web.Scotty as Scotty

import Api.Resources.Error.ErrorDTO
import Common.Error
import Service.Token.TokenService

getReqDto callback = do
  body <- Scotty.body
  let eitherReqDto = eitherDecode body
  case eitherReqDto of
    Right reqDto -> callback reqDto
    Left error -> sendError $ createErrorWithErrorMessage error

getCurrentUserUuid context callback = do
  tokenHeader <- Scotty.header "Authorization"
  let userUuidMaybe =
        getUserUuidFromToken
          context
          (tokenHeader >>= \token -> Just . LT.toStrict $ token) :: Maybe T.Text
  case userUuidMaybe of
    Just userUuid -> callback (T.unpack userUuid)
    Nothing -> unauthorizedA

getQueryParam paramName = do
  params <- Scotty.params
  let mValue = lookup paramName params
  case mValue of
    Just value -> return . Just . LT.toStrict $ value
    Nothing -> return Nothing

getListOfQueryParamsIfPresent :: [LT.Text] -> Scotty.ActionM [(T.Text, T.Text)]
getListOfQueryParamsIfPresent = Prelude.foldr go (return [])
  where
    go name monadAcc = do
      value <- extractQueryParam name
      acc <- monadAcc
      return $ maybeToList value ++ acc
    extractQueryParam name = do
      mValue <- getQueryParam name
      case mValue of
        Just value -> return $ Just (LT.toStrict name, value)
        Nothing -> return Nothing

checkPermission context perm callback = do
  tokenHeader <- Scotty.header "Authorization"
  let mUserPerms =
        getPermissionsFromToken
          context
          (tokenHeader >>= \token -> Just . LT.toStrict $ token)
  case mUserPerms of
    Just userPerms ->
      if perm `Prelude.elem` userPerms
        then callback
        else forbidden
    Nothing -> forbidden

sendJson obj = do
  Scotty.setHeader (LT.pack "Content-Type") (LT.pack "application/json")
  Scotty.raw $ encode obj

sendError :: AppError -> Scotty.ActionM ()
sendError (ValidationError errorMessage formErrors fieldErrors) = do
  Scotty.status badRequest400
  sendJson $ ValidationError errorMessage formErrors fieldErrors
sendError (ForbiddenError errorMessage) = do
  Scotty.status unauthorized401
  sendJson $ ForbiddenError errorMessage
sendError (NotExistsError errorMessage) = do
  Scotty.status notFound404
  sendJson $ NotExistsError errorMessage
sendError (DatabaseError errorMessage) = do
  Scotty.status internalServerError500
  sendJson $ DatabaseError errorMessage

unauthorizedA :: Scotty.ActionM ()
unauthorizedA = do
  Scotty.status unauthorized401
  sendJson $ object ["status" .= 401, "error" .= "Unauthorized"]

unauthorizedL :: Response
unauthorizedL =
  responseLBS unauthorized401 [(hContentType, "application/json")] $
  encode (object ["status" .= 401, "error" .= "Unauthorized"])

forbidden :: Scotty.ActionM ()
forbidden = do
  Scotty.status forbidden403
  sendJson $ object ["status" .= 403, "error" .= "Forbidden"]

notFoundA :: Scotty.ActionM ()
notFoundA = do
  request <- Scotty.request
  if requestMethod request == methodOptions
    then Scotty.status ok200
    else do
      Scotty.status notFound404
      sendJson $ object ["status" .= 404, "error" .= "Not Found"]
