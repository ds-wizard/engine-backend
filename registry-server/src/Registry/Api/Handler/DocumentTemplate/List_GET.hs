module Registry.Api.Handler.DocumentTemplate.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.DocumentTemplate.DocumentTemplateService
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])
list_GET mTokenHeader mOrganizationId mTmlId mMetamodelVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "template_id" <$> mTmlId]
        getDocumentTemplates queryParams mMetamodelVersion
