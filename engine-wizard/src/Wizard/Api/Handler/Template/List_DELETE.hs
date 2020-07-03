module Wizard.Api.Handler.Template.List_DELETE where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_DELETE
   = Header "Authorization" String
     :> "templates"
     :> QueryParam "organizationId" String
     :> QueryParam "templateId" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_DELETE ::
     Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mOrganizationId mTemplateId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "organizationId" <$> mOrganizationId, (,) "templateId" <$> mTemplateId]
      deleteTemplatesByQueryParams queryParams
      return NoContent
