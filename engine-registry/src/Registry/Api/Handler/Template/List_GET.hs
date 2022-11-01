module Registry.Api.Handler.Template.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Api.Resource.Template.TemplateSimpleJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Template.TemplateService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_GET
   = Header "Authorization" String
     :> "templates"
     :> QueryParam "organizationId" String
     :> QueryParam "templateId" String
     :> QueryParam "metamodelVersion" Int
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [TemplateSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [TemplateSimpleDTO])
list_GET mTokenHeader mOrganizationId mTmlId mMetamodelVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "template_id" <$> mTmlId]
      getTemplates queryParams mMetamodelVersion
