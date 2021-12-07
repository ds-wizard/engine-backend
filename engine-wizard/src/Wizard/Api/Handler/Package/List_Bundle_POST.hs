module Wizard.Api.Handler.Package.List_Bundle_POST where

import qualified Data.List as L
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.PackageBundle.PackageBundleService

type List_Bundle_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> MultipartForm Mem (MultipartData Mem)
     :> "packages"
     :> "bundle"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])

list_bundle_POST ::
     Maybe String
  -> Maybe String
  -> MultipartData Mem
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])
list_bundle_POST mTokenHeader mServerUrl multipartData =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let fs = files multipartData
      case L.find (\file -> fdInputName file == "file") fs of
        Just file -> do
          let content = fdPayload file
          importAndConvertPackageBundle content
        Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
