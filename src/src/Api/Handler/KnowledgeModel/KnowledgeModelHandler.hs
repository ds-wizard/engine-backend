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

getKnowledgeModelA :: Context -> DSPConfig -> Scotty.ActionM ()
getKnowledgeModelA context dspConfig = do
  kmcUuid <- Scotty.param "kmcUuid"
  maybeDto <- liftIO $ getKnowledgeModelByKmcId context kmcUuid
  case maybeDto of
    Just dto -> sendJson dto
    Nothing -> notFoundA
