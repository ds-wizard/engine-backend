module Wizard.Api.Handler.Project.File.Detail_Download_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.File.ProjectFileService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()

type Detail_Download_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_download_GET :: Maybe String -> Maybe String -> U.UUID -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_download_GET mTokenHeader mServerUrl projectUuid fileUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< downloadProjectFile projectUuid fileUuid
