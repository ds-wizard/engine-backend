module Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateJM ()
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService

type Detail_Locales_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem KnowledgeModelLocaleCreateDTO
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelLocaleList)

detail_locales_POST
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelLocaleCreateDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelLocaleList)
detail_locales_POST mTokenHeader mServerUrl reqDto pkgUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createLocale pkgUuid reqDto
