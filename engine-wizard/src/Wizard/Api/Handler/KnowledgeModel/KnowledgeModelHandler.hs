module Wizard.Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Wizard.Service.KnowledgeModel.KnowledgeModelService

postKnowledgeModelPreviewA :: Endpoint
postKnowledgeModelPreviewA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      eitherResDto <- runInAuthService $ createKnowledgeModelPreview reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error
