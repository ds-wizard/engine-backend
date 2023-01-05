module Wizard.Api.Handler.Package.List_Bundle_POST where

import qualified Data.List as L
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.PackageBundle.PackageBundleFileDTO
import Wizard.Api.Resource.PackageBundle.PackageBundleFileJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Owl.OwlService
import Wizard.Service.PackageBundle.PackageBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem PackageBundleFileDTO
    :> "packages"
    :> "bundle"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [PackageSimpleDTO])

list_bundle_POST
  :: Maybe String
  -> Maybe String
  -> PackageBundleFileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [PackageSimpleDTO])
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< if L.isSuffixOf ".ttl" reqDto.fileName || L.isSuffixOf ".owl" reqDto.fileName
          then importOwl reqDto
          else importAndConvertPackageBundle reqDto.content False
