module Wizard.Api.Handler.Package.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSuggestionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Package.PackageSuggestion
import Wizard.Service.Package.PackageService

type List_Suggestions_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "packages"
     :> "suggestions"
     :> QueryParam "q" String
     :> QueryParam "select" String
     :> QueryParam "exclude" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page PackageSuggestion))

list_suggestions_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page PackageSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelect mExclude mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
    addTraceUuidHeader =<< do
      let mSelectIds = fmap (splitOn ",") mSelect
      let mExcludeIds = fmap (splitOn ",") mExclude
      getPackageSuggestions mQuery mSelectIds mExcludeIds (Pageable mPage mSize) (parseSortQuery mSort)
