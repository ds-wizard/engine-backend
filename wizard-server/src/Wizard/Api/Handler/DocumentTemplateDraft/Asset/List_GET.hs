module Wizard.Api.Handler.DocumentTemplateDraft.Asset.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "assets"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateAssetDTO])

list_GET
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateAssetDTO])
list_GET mTokenHeader mServerUrl tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAssets tmlId
