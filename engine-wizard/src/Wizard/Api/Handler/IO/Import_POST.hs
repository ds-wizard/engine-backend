module Wizard.Api.Handler.IO.Import_POST where

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

type Import_POST
   = Header "Authorization" String
     :> MultipartForm Mem (MultipartData Mem)
     :> "import"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])

import_POST ::
     Maybe String -> MultipartData Mem -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])
import_POST mTokenHeader multipartData =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let fs = files multipartData
      case L.find (\file -> fdInputName file == "file") fs of
        Just file -> do
          let content = fdPayload file
          importAndConvertPackageBundle content
        Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
