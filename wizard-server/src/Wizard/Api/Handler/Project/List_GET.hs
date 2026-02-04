module Wizard.Api.Handler.Project.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.String (splitOn)
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> QueryParam "q" String
    :> QueryParam "isTemplate" Bool
    :> QueryParam "isMigrating" Bool
    :> QueryParam "projectTags" String
    :> QueryParam "projectTagsOp" String
    :> QueryParam "userUuids" String
    :> QueryParam "userUuidsOp" String
    :> QueryParam "knowledgeModelPackageIds" [Coordinate]
    :> QueryParam "knowledgeModelPackageIdsOp" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectDTO))

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
  -> Maybe [Coordinate]
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectDTO))
list_GET mTokenHeader mServerUrl mQuery mIsTemplate mIsMigrating mProjectTagsL mProjectTagsOp mUserUuidsL mUserUuidsOp mKnowledgeModelPackageCoordinates mKnowledgeModelPackageCoordinatesOp mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mUserUuids = fmap (splitOn ",") mUserUuidsL
        let mProjectTags = fmap (splitOn ",") mProjectTagsL
        getProjectsForCurrentUserPageDto
          mQuery
          mIsTemplate
          mIsMigrating
          mProjectTags
          mProjectTagsOp
          mUserUuids
          mUserUuidsOp
          mKnowledgeModelPackageCoordinates
          mKnowledgeModelPackageCoordinatesOp
          (Pageable mPage mSize)
          (parseSortQuery mSort)
