module Api.Handler.KnowledgeModelContainer.KnowledgeModelContainerHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Context
import DSPConfig
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

getKnowledgeModelContainersA :: Context -> DSPConfig -> Scotty.ActionM ()
getKnowledgeModelContainersA context dspConfig =
  checkPermission context "KM_PERM" $ do
    eitherDtos <- liftIO $ getKnowledgeModelContainers context
    case eitherDtos of
      Right dtos -> sendJson dtos
      Left error -> sendError error

postKnowledgeModelContainersA :: Context -> DSPConfig -> Scotty.ActionM ()
postKnowledgeModelContainersA context dspConfig =
  checkPermission context "KM_PERM" $
  getReqDto $ \reqDto -> do
    eitherResDto <- liftIO $ createKnowledgeModelContainer context reqDto
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> do
        Scotty.status created201
        sendJson resDto

getKnowledgeModelContainerA :: Context -> DSPConfig -> Scotty.ActionM ()
getKnowledgeModelContainerA context dspConfig =
  checkPermission context "KM_PERM" $ do
    kmcUuid <- Scotty.param "kmcUuid"
    eitherResDto <- liftIO $ getKnowledgeModelContainerById context kmcUuid
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> sendJson resDto

putKnowledgeModelContainerA :: Context -> DSPConfig -> Scotty.ActionM ()
putKnowledgeModelContainerA context dspConfig =
  checkPermission context "KM_PERM" $
  getReqDto $ \reqDto -> do
    kmcUuid <- Scotty.param "kmcUuid"
    eitherResDto <-
      liftIO $ modifyKnowledgeModelContainer context kmcUuid reqDto
    case eitherResDto of
      Left appError -> sendError appError
      Right resDto -> sendJson resDto

deleteKnowledgeModelContainerA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteKnowledgeModelContainerA context dspConfig =
  checkPermission context "KM_PERM" $ do
    kmcUuid <- Scotty.param "kmcUuid"
    maybeError <- liftIO $ deleteKnowledgeModelContainer context kmcUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
