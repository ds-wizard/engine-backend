module Wizard.Api.Handler.Questionnaire.File.List_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.File.FileCreateDTO
import Wizard.Api.Resource.File.FileCreateJM ()
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Service.Questionnaire.File.QuestionnaireFileService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileCreateDTO
    :> "questionnaires"
    :> Capture "uuid" U.UUID
    :> "files"
    :> Capture "questionUuid" U.UUID
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireFileList)

list_POST :: Maybe String -> Maybe String -> FileCreateDTO -> U.UUID -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireFileList)
list_POST mTokenHeader mServerUrl reqDto qtnUuid questionUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        createQuestionnaireFile qtnUuid questionUuid reqDto
