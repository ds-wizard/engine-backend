module Wizard.Api.Handler.KnowledgeModelEditor.Detail_Locales_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService

type Detail_Locales_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelLocaleList])

detail_locales_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelLocaleList])
detail_locales_GET mTokenHeader mServerUrl editorUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getReusableLocalesForEditor editorUuid
