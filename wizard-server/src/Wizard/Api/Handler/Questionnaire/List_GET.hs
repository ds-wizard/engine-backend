module Wizard.Api.Handler.Questionnaire.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> QueryParam "q" String
    :> QueryParam "isTemplate" Bool
    :> QueryParam "isMigrating" Bool
    :> QueryParam "projectTags" String
    :> QueryParam "projectTagsOp" String
    :> QueryParam "userUuids" String
    :> QueryParam "userUuidsOp" String
    :> QueryParam "knowledgeModelPackageIds" String
    :> QueryParam "knowledgeModelPackageIdsOp" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page QuestionnaireDTO))
list_GET mTokenHeader mServerUrl mQuery mIsTemplate mIsMigrating mProjectTagsL mProjectTagsOp mUserUuidsL mUserUuidsOp mKnowledgeModelPackageIdsL mKnowledgeModelPackageIdsOp mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mUserUuids = fmap (splitOn ",") mUserUuidsL
        let mProjectTags = fmap (splitOn ",") mProjectTagsL
        let mKnowledgeModelPackageIds = fmap (splitOn ",") mKnowledgeModelPackageIdsL
        getQuestionnairesForCurrentUserPageDto
          mQuery
          mIsTemplate
          mIsMigrating
          mProjectTags
          mProjectTagsOp
          mUserUuids
          mUserUuidsOp
          mKnowledgeModelPackageIds
          mKnowledgeModelPackageIdsOp
          (Pageable mPage mSize)
          (parseSortQuery mSort)
