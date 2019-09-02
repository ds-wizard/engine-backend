module Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Web.Scotty.Trans (json)

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Service.KnowledgeModel.KnowledgeModelService

postKnowledgeModelPreviewA :: Endpoint
postKnowledgeModelPreviewA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherResDto <- runInAuthService $ createKnowledgeModelPreview reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error
