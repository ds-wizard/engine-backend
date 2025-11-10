module Wizard.Api.Handler.KnowledgeModelPackage.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.String (splitOn)
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "select" String
    :> QueryParam "exclude" String
    :> QueryParam "phase" KnowledgeModelPackagePhase
    :> QueryParam "nonEditable" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe KnowledgeModelPackagePhase
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelect mExclude mPhase mNonEditable mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mSelectIds = fmap (splitOn ",") mSelect
        let mExcludeIds = fmap (splitOn ",") mExclude
        getPackageSuggestions mQuery mSelectIds mExcludeIds mPhase mNonEditable (Pageable mPage mSize) (parseSortQuery mSort)
