module Wizard.Api.Handler.Template.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_Suggestions_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> "suggestions"
     :> QueryParam "pkgId" String
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page TemplateSuggestionDTO))

list_suggestions_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page TemplateSuggestionDTO))
list_suggestions_GET mTokenHeader mServerUrl mPkgId mQuery mPage mSize mSort =
  getServiceTokenOrAuthServiceExecutor mTokenHeader mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< getTemplateSuggestions mPkgId mQuery (Pageable mPage mSize) (parseSortQuery mSort)
