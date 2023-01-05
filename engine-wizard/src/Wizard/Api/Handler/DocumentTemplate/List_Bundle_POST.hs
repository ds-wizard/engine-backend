module Wizard.Api.Handler.DocumentTemplate.List_Bundle_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO hiding (files)
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM ()
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "document-templates"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateBundleDTO)

list_bundle_POST
  :: Maybe String
  -> Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateBundleDTO)
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importAndConvertBundle reqDto.content False
