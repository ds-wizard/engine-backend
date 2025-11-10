module Wizard.Api.Handler.KnowledgeModelPackage.List_Bundle_POST where

import qualified Data.List as L
import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleFileJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile
import Wizard.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Wizard.Service.Owl.OwlService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem KnowledgeModelBundleFile
    :> "knowledge-model-packages"
    :> "bundle"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])

list_bundle_POST
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelBundleFile
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< if L.isSuffixOf ".ttl" reqDto.fileName || L.isSuffixOf ".owl" reqDto.fileName
          then importOwl reqDto
          else importAndConvertBundle reqDto.content False
