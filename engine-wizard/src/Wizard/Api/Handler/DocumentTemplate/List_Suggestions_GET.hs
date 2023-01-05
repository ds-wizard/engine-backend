module Wizard.Api.Handler.DocumentTemplate.List_Suggestions_GET where

import Data.Maybe (fromMaybe)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplatePhaseJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.DocumentTemplateService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> "suggestions"
    :> QueryParam "pkgId" String
    :> QueryParam "includeUnsupportedMetamodelVersion" Bool
    :> QueryParam "phase" DocumentTemplatePhase
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateSuggestionDTO))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe DocumentTemplatePhase
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateSuggestionDTO))
list_suggestions_GET mTokenHeader mServerUrl mPkgId mIncludeUnsupportedMetamodelVersion mPhase mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let includeUnsupportedMetamodelVersion = fromMaybe False mIncludeUnsupportedMetamodelVersion
        getDocumentTemplateSuggestions mPkgId includeUnsupportedMetamodelVersion mPhase mQuery (Pageable mPage mSize) (parseSortQuery mSort)
