module Wizard.Api.Handler.Template.List_All_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_All_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "templates"
    :> "all"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TemplateSuggestionDTO])

list_all_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TemplateSuggestionDTO])
list_all_GET mTokenHeader mServerUrl mOrganizationId mTmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "template_id" <$> mTmlId]
        getTemplatesDto queryParams
