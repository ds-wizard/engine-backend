module Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Service.KnowledgeModel.KnowledgeModelService

getKnowledgeModelA :: Endpoint
getKnowledgeModelA =
  checkPermission "KM_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    branchUuid <- param "branchUuid"
    eitherDto <- runInAuthService $ getKnowledgeModelByBranchId branchUuid
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error

postKnowledgeModelPreviewA :: Endpoint
postKnowledgeModelPreviewA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherResDto <- runInAuthService $ createKnowledgeModelPreview reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error
