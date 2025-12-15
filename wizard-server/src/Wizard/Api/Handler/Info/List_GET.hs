module Wizard.Api.Handler.Info.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Api.Resource.Info.InfoJM ()
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Service.Info.InfoService
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Wizard.Api.Handler.Common
import Wizard.Constant.ProjectAction
import Wizard.Constant.ProjectImporter
import Wizard.Model.Context.BaseContext

type List_GET =
  Header "Host" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< do
      let metamodelVersions =
            [ InfoMetamodelVersionDTO {name = "Knowledge Model", version = show knowledgeModelMetamodelVersion}
            , InfoMetamodelVersionDTO {name = "Document Template", version = show documentTemplateMetamodelVersion}
            , InfoMetamodelVersionDTO {name = "Project Importer", version = show projectImporterMetamodelVersion}
            , InfoMetamodelVersionDTO {name = "Project Action", version = show projectActionMetamodelVersion}
            ]
      getInfo metamodelVersions
