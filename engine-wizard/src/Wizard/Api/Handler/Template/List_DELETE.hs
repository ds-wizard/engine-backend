module Wizard.Api.Handler.Template.List_DELETE where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl mOrganizationId mTemplateId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "template_id" <$> mTemplateId]
        deleteTemplatesByQueryParams queryParams
        return NoContent
