module Registry.Api.Handler.DocumentTemplate.List_Bundle_POST where

import Servant
import Servant.Multipart

import Registry.Api.Handler.Common
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService
import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Common.FileDTO
import Shared.Common.Api.Resource.Common.FileJM ()
import Shared.Common.Model.Context.TransactionState

type List_Bundle_POST =
  Header "Authorization" String
    :> MultipartForm Mem FileDTO
    :> "document-templates"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)

list_bundle_POST
  :: Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< importBundle reqDto.content
