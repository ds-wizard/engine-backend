module Wizard.Api.Handler.Questionnaire.ProjectTag.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.ProjectTag.ProjectTagService

type List_Suggestions_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> "project-tags"
     :> "suggestions"
     :> QueryParam "q" String
     :> QueryParam "exclude" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page String))

list_suggestions_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page String))
list_suggestions_GET mTokenHeader mServerUrl mQuery mExclude mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
    addTraceUuidHeader =<< do
      let excludeTags = maybe [] (splitOn ",") mExclude
      getProjectTagSuggestions mQuery excludeTags (Pageable mPage mSize) (parseSortQuery mSort)
