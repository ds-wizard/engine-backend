module Wizard.Api.Handler.Questionnaire.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> QueryParam "q" String
     :> QueryParam "isTemplate" Bool
     :> QueryParam "userUuids" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page QuestionnaireDTO))

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page QuestionnaireDTO))
list_GET mTokenHeader mServerUrl mQuery mIsTemplate mUserUuidsL mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let mUserUuids = fmap (splitOn ",") mUserUuidsL
      getQuestionnairesForCurrentUserPageDto mQuery mIsTemplate mUserUuids (Pageable mPage mSize) (parseSortQuery mSort)
