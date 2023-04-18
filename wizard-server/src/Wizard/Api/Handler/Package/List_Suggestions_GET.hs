module Wizard.Api.Handler.Package.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSuggestionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Package.PackageSuggestion
import Wizard.Service.Package.PackageService
import WizardLib.KnowledgeModel.Model.Package.Package

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "packages"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "select" String
    :> QueryParam "exclude" String
    :> QueryParam "phase" PackagePhase
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page PackageSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe PackagePhase
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page PackageSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelect mExclude mPhase mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mSelectIds = fmap (splitOn ",") mSelect
        let mExcludeIds = fmap (splitOn ",") mExclude
        getPackageSuggestions mQuery mSelectIds mExcludeIds mPhase (Pageable mPage mSize) (parseSortQuery mSort)
