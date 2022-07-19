module Wizard.Api.Handler.KnowledgeModel.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService

type List_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] KnowledgeModelChangeDTO
     :> "knowledge-models"
     :> "preview"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] KnowledgeModel)

list_POST ::
     Maybe String
  -> Maybe String
  -> KnowledgeModelChangeDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] KnowledgeModel)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< createKnowledgeModelPreview reqDto
