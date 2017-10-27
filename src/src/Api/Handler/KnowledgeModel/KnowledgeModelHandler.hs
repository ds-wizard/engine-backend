module Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Context
import DSPConfig
import Service.KnowledgeModel.KnowledgeModelService

getKnowledgeModelsA :: Context -> DSPConfig -> Scotty.ActionM ()
getKnowledgeModelsA context dspConfig = do
  dtos <- liftIO $ getKnowledgeModels context
  let a = dtos :: [KnowledgeModelDTO]
  Scotty.json dtos

getKnowledgeModelA :: Context -> DSPConfig -> Scotty.ActionM ()
getKnowledgeModelA context dspConfig = do
  kmUuid <- Scotty.param "kmUuid"
  maybeDto <- liftIO $ getKnowledgeModelById context kmUuid
  case maybeDto of
    Just dto -> Scotty.json dto
    Nothing -> notFoundA
