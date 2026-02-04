module Wizard.Api.Handler.KnowledgeModelPackage.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate
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
    :> QueryParam "select" [Coordinate]
    :> QueryParam "exclude" [Coordinate]
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
  -> Maybe [Coordinate]
  -> Maybe [Coordinate]
  -> Maybe KnowledgeModelPackagePhase
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        getPackageSuggestions mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable (Pageable mPage mSize) (parseSortQuery mSort)
