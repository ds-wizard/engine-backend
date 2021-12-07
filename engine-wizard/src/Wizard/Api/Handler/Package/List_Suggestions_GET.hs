module Wizard.Api.Handler.Package.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Api.Resource.Package.PackageSuggestionJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type List_Suggestions_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "packages"
     :> "suggestions"
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page PackageSuggestionDTO))

list_suggestions_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page PackageSuggestionDTO))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getPackageSuggestions mQuery (Pageable mPage mSize) (parseSortQuery mSort)
