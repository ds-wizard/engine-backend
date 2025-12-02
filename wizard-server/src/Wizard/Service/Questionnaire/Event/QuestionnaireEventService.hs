module Wizard.Service.Questionnaire.Event.QuestionnaireEventService where

import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List (groupBy)
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireVersion

squashQuestionnaireEvents :: AppContextM ()
squashQuestionnaireEvents = do
  qtnUuids <- findQuestionnaireForSquashing
  traverse_ squashQuestionnaireEventsForQuestionnaire qtnUuids

squashQuestionnaireEventsForQuestionnaire :: U.UUID -> AppContextM ()
squashQuestionnaireEventsForQuestionnaire qtnUuid =
  runInTransaction $ do
    logInfoI _CMP_SERVICE (f' "Squashing events for questionnaire (qtnUuid: '%s')" [U.toString qtnUuid])
    events <- findQuestionnaireEventsByQuestionnaireUuid qtnUuid
    versions <- findQuestionnaireVersionsByQuestionnaireUuid qtnUuid
    let squashedEvents = squash versions events
    syncQuestionnaireEventsWithDb events squashedEvents
    updateQuestionnaireSquashedByUuid qtnUuid True
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for questionnaire '%s' finished successfully (before: %s, after %s)"
          [U.toString qtnUuid, show . length $ events, show . length $ squashedEvents]
      )

instance Ord QuestionnaireEvent where
  compare a b = compare (getCreatedAt a) (getCreatedAt b)

squash :: [QuestionnaireVersion] -> [QuestionnaireEvent] -> [QuestionnaireEvent]
squash versions events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (getCreatedAt e1) == utctDay (getCreatedAt e2)) events
      squashedEvents = fmap (squashOnePeriod versions) groupedEvents
   in concat squashedEvents

squashOnePeriod :: [QuestionnaireVersion] -> [QuestionnaireEvent] -> [QuestionnaireEvent]
squashOnePeriod versions = snd . foldr go (M.empty, [])
  where
    go
      :: QuestionnaireEvent
      -> (M.Map String (Maybe U.UUID), [QuestionnaireEvent])
      -> (M.Map String (Maybe U.UUID), [QuestionnaireEvent])
    go event' (questions, events) =
      case event' of
        SetReplyEvent' event ->
          if not (L.any (\v -> v.eventUuid == event.uuid) versions)
            && Just event.createdBy == M.lookup event.path questions
            then (questions, events)
            else (M.insert event.path event.createdBy questions, event' : events)
        _ -> (questions, event' : events)
