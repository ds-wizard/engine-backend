module Wizard.Api.Handler.Document.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentCreateJM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] DocumentCreateDTO
     :> "documents"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] DocumentDTO)

list_POST :: Maybe String -> DocumentCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] DocumentDTO)
list_POST mTokenHeader reqDto =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $ addTraceUuidHeader =<< createDocument reqDto
