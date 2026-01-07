module Wizard.Service.Project.Event.ProjectEventService where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List (groupBy)
import Shared.Common.Util.Logger
import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Project
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.User.UserMapper

addEventToProject :: U.UUID -> ProjectEventChangeDTO -> AppContextM ()
addEventToProject projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkEditPermissionToProject project.visibility project.sharing project.permissions
    mCurrentUser <- asks currentUser
    let mCreatedBy = fmap toSuggestion' mCurrentUser
    addEvent projectUuid EditorWebsocketPerm mCreatedBy reqDto

squashProjectEvents :: AppContextM ()
squashProjectEvents = do
  projectUuids <- findProjectForSquashing
  traverse_ squashProjectEventsForProject projectUuids

squashProjectEventsForProject :: U.UUID -> AppContextM ()
squashProjectEventsForProject projectUuid =
  runInTransaction $ do
    logInfoI _CMP_SERVICE (f' "Squashing events for project (projectUuid: '%s')" [U.toString projectUuid])
    events <- findProjectEventsByProjectUuid projectUuid
    versions <- findProjectVersionsByProjectUuid projectUuid
    let squashedEvents = squash versions events
    syncProjectEventsWithDb events squashedEvents
    updateProjectSquashedByUuid projectUuid True
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for project '%s' finished successfully (before: %s, after %s)"
          [U.toString projectUuid, show . length $ events, show . length $ squashedEvents]
      )

instance Ord ProjectEvent where
  compare a b = compare (getCreatedAt a) (getCreatedAt b)

squash :: [ProjectVersion] -> [ProjectEvent] -> [ProjectEvent]
squash versions events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (getCreatedAt e1) == utctDay (getCreatedAt e2)) events
      squashedEvents = fmap (squashOnePeriod versions) groupedEvents
   in concat squashedEvents

squashOnePeriod :: [ProjectVersion] -> [ProjectEvent] -> [ProjectEvent]
squashOnePeriod versions = snd . foldr go (M.empty, [])
  where
    go
      :: ProjectEvent
      -> (M.Map String (Maybe U.UUID), [ProjectEvent])
      -> (M.Map String (Maybe U.UUID), [ProjectEvent])
    go event' (questions, events) =
      case event' of
        SetReplyEvent' event ->
          if not (L.any (\v -> v.eventUuid == event.uuid) versions)
            && Just event.createdBy == M.lookup event.path questions
            then (questions, events)
            else (M.insert event.path event.createdBy questions, event' : events)
        _ -> (questions, event' : events)
