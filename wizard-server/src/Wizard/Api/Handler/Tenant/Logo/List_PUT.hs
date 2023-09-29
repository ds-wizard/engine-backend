module Wizard.Api.Handler.Tenant.Logo.List_PUT where

import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Common.FileDTO
import Shared.Common.Api.Resource.Common.FileJM ()
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Logo.LogoService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "tenants"
    :> "current"
    :> "logo"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_PUT
  :: Maybe String
  -> Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        uploadLogo reqDto.fileName reqDto.contentType reqDto.content
        return NoContent
