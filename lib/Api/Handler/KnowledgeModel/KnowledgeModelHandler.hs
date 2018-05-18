module Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Control.Monad.Trans.Class (lift)
import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Service.KnowledgeModel.KnowledgeModelService

getKnowledgeModelA :: Endpoint
getKnowledgeModelA =
  checkPermission "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherDto <- lift $ getKnowledgeModelByBranchId branchUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error
