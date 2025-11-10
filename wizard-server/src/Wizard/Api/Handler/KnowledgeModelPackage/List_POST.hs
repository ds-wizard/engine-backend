module Wizard.Api.Handler.KnowledgeModelPackage.List_POST where

import Servant

import qualified Data.ByteString.Lazy.Char8 as BSL
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[JSONPlain] String
    :> "knowledge-model-packages"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])

list_POST
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])
list_POST mTokenHeader mServerUrl reqBody =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importAndConvertBundle (BSL.pack reqBody) False
