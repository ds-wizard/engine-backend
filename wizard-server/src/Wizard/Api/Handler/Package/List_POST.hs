module Wizard.Api.Handler.Package.List_POST where

import Servant

import qualified Data.ByteString.Lazy.Char8 as BSL
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.Bundle.PackageBundleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[JSONPlain] String
    :> "packages"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [PackageSimpleDTO])

list_POST
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [PackageSimpleDTO])
list_POST mTokenHeader mServerUrl reqBody =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importAndConvertBundle (BSL.pack reqBody) False
