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
import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerCreateDTO
import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Context
import DSPConfig
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

-- getKnowledgeModelContainersA :: Context -> DSPConfig -> Scotty.ActionM ()
-- getKnowledgeModelContainersA context dspConfig = do
--   dtos <- liftIO $ getKnowledgeModelContainers context
--   let a = dtos :: [KnowledgeModelContainerDTO]
--   Scotty.json dtos

postKnowledgeModelContainersA :: Context -> DSPConfig -> Scotty.ActionM ()
postKnowledgeModelContainersA context dspConfig = do
  kmcCreateDto <- Scotty.jsonData
  kmcDto <- liftIO $ createKnowledgeModelContainer context kmcCreateDto
  Scotty.json kmcDto

-- getKnowledgeModelContainerA :: Context -> DSPConfig -> Scotty.ActionM ()
-- getKnowledgeModelContainerA context dspConfig = do
--   kmcUuid <- Scotty.param "kmcUuid"
--   maybeDto <- liftIO $ getKnowledgeModelContainerById context kmcUuid
--   case maybeDto of
--     Just dto -> Scotty.json dto
--     Nothing -> notFoundA

-- putKnowledgeModelContainerA :: Context -> DSPConfig -> Scotty.ActionM ()
-- putKnowledgeModelContainerA context dspConfig = do
--   kmcUuid <- Scotty.param "kmcUuid"
--   kmcDto <- Scotty.jsonData
--   maybeDto <- liftIO $ modifyKnowledgeModelContainer context kmcUuid kmcDto
--   case maybeDto of
--     Just dto -> Scotty.json dto
--     Nothing -> notFoundA

-- deleteKnowledgeModelContainerA :: Context -> DSPConfig -> Scotty.ActionM ()
-- deleteKnowledgeModelContainerA context dspConfig = do
--   kmcUuid <- Scotty.param "kmcUuid"
--   isSuccess <- liftIO $ deleteKnowledgeModelContainer context kmcUuid
--   if isSuccess
--     then Scotty.status noContent204
--     else notFoundA
