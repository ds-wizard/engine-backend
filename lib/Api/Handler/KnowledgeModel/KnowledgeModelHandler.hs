module Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy
import Data.UUID
import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.Context.AppContext
import Service.KnowledgeModel.KnowledgeModelService

getKnowledgeModelA :: Endpoint
getKnowledgeModelA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherDto <- liftIO $ getKnowledgeModelByBranchId context branchUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error
