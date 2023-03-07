module Wizard.Api.Handler.Document.Detail_Download_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO
import Wizard.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type Detail_Download_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> Capture "docUuid" U.UUID
    :> "download"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_download_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_download_GET mTokenHeader mServerUrl docUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< downloadDocument docUuid
