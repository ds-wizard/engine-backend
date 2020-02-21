module Wizard.Api.Handler.KnowledgeModel.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] KnowledgeModelChangeDTO
     :> "knowledge-models"
     :> "preview"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] KnowledgeModelDTO)

list_POST ::
     Maybe String
  -> KnowledgeModelChangeDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] KnowledgeModelDTO)
list_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "QTN_PERM"
      createKnowledgeModelPreview reqDto
