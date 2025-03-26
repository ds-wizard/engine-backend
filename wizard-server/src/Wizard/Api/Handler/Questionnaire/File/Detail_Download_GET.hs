module Wizard.Api.Handler.Questionnaire.File.Detail_Download_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.File.QuestionnaireFileService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()

type Detail_Download_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "questionnaireUuid" U.UUID
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_download_GET :: Maybe String -> Maybe String -> U.UUID -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_download_GET mTokenHeader mServerUrl qtnUuid fileUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< downloadQuestionnaireFile qtnUuid fileUuid
