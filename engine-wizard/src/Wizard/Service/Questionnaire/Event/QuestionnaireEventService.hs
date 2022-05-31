module Wizard.Service.Questionnaire.Event.QuestionnaireEventService where

import Control.Lens ((^.))
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig hiding (squash)
import Shared.Model.Common.Lens
import Shared.Util.List (groupBy)
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSquash
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Util.Logger

squashQuestionnaireEvents :: AppContextM ()
squashQuestionnaireEvents = do
  qtnUuids <- findQuestionnaireForSquashing
  traverse_ (squashQuestionnaireEventsForQuestionnaire . U.toString) qtnUuids

squashQuestionnaireEventsForQuestionnaire :: String -> AppContextM ()
squashQuestionnaireEventsForQuestionnaire qtnUuid =
  runInTransaction $ do
    logInfoU _CMP_SERVICE (f' "Squashing events for questionnaire (qtnUuid: '%s')" [qtnUuid])
    (QuestionnaireSquash _ events versions) <- findQuestionnaireSquashById qtnUuid
    let squashedEvents = squash versions events
    updateQuestionnaireEventsByUuid qtnUuid True squashedEvents
    logInfoU
      _CMP_SERVICE
      (f'
         "Squashing for questionnaire '%s' finished successfully (before: %s, after %s)"
         [qtnUuid, show . length $ events, show . length $ squashedEvents])

instance Ord QuestionnaireEvent where
  compare a b = compare (a ^. createdAt') (b ^. createdAt')

squash :: [QuestionnaireVersion] -> [QuestionnaireEvent] -> [QuestionnaireEvent]
squash versions events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (e1 ^. createdAt') == utctDay (e2 ^. createdAt')) events
      squashedEvents = fmap (squashOnePeriod versions) groupedEvents
   in concat squashedEvents

squashOnePeriod :: [QuestionnaireVersion] -> [QuestionnaireEvent] -> [QuestionnaireEvent]
squashOnePeriod versions = snd . foldr go (M.empty, [])
  where
    go ::
         QuestionnaireEvent
      -> (M.Map String (Maybe U.UUID), [QuestionnaireEvent])
      -> (M.Map String (Maybe U.UUID), [QuestionnaireEvent])
    go event' (questions, events) =
      case event' of
        SetReplyEvent' event ->
          if not (L.any (\v -> v ^. eventUuid == event ^. uuid) versions) &&
             Just (event ^. createdBy) == M.lookup (event ^. path) questions
            then (questions, events)
            else (M.insert (event ^. path) (event ^. createdBy) questions, event' : events)
        _ -> (questions, event' : events)
