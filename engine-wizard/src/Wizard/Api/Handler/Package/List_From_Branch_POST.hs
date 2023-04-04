module Wizard.Api.Handler.Package.List_From_Branch_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.Publish.PackagePublishService

type List_From_Branch_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PackagePublishBranchDTO
    :> "packages"
    :> "from-branch"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageSimpleDTO)

list_from_branch_POST
  :: Maybe String
  -> Maybe String
  -> PackagePublishBranchDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageSimpleDTO)
list_from_branch_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< publishPackageFromBranch reqDto
