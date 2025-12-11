module Wizard.Api.Handler.Project.File.List_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.File.FileCreateDTO
import Wizard.Api.Resource.File.FileCreateJM ()
import Wizard.Api.Resource.Project.File.ProjectFileListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.File.ProjectFileList
import Wizard.Service.Project.File.ProjectFileService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileCreateDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "files"
    :> Capture "questionUuid" U.UUID
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectFileList)

list_POST :: Maybe String -> Maybe String -> FileCreateDTO -> U.UUID -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectFileList)
list_POST mTokenHeader mServerUrl reqDto projectUuid questionUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        createProjectFile projectUuid questionUuid reqDto
