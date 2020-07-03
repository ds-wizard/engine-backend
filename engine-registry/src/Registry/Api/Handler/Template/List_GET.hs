module Registry.Api.Handler.Template.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Api.Resource.Template.TemplateSimpleJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Template.TemplateService
import Shared.Api.Handler.Common

type List_GET
   = Header "Authorization" String
     :> "templates"
     :> QueryParam "organizationId" String
     :> QueryParam "templateId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [TemplateSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [TemplateSimpleDTO])
list_GET mTokenHeader mOrganizationId mTmlId =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "organizationId" <$> mOrganizationId, (,) "templateId" <$> mTmlId]
      getTemplates queryParams
