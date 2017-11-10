module Database.Migration.KnowledgeModel.KnowledgeModelContainerMigration where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Context
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Database.Migration.KnowledgeModel.Data.Event.Event
import Model.Event.Event
import Service.Event.EventService
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

runMigration context dspConfig logState = do
  logState "MIGRATION (KnowledgeModel/KnowledgeModelContainer): started"
  deleteKnowledgeModelContainers context
  let kmc =
        KnowledgeModelContainerDTO
        { _kmcdtoKmContainerUuid =
            (fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6"))
        , _kmcdtoName = "KM Container from Amsterdam"
        , _kmcdtoShortName = "elixir-nl-ams"
        , _kmcdtoParentPackageName = "elixir-nl"
        , _kmcdtoParentPackageVersion = "1.0.0"
        }
  createKnowledgeModelContainer context kmc
  let events =
        [ AddQuestionEvent' a_km1_ch1_q1
        , AddQuestionEvent' a_km1_ch1_q2
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1
        , AddAnswerEvent' a_km1_ch1_q2_aNo3
        , AddAnswerEvent' a_km1_ch1_q2_aYes3
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        , AddAnswerEvent' a_km1_ch1_q2_aNo4
        , AddAnswerEvent' a_km1_ch1_q2_aYes4
        , AddExpertEvent' a_km1_ch1_q2_eDarth
        , AddExpertEvent' a_km1_ch1_q2_eLuke
        , AddReferenceEvent' a_km1_ch1_q2_rCh1
        , AddReferenceEvent' a_km1_ch1_q2_rCh2
        , AddChapterEvent' a_km1_ch2
        , AddQuestionEvent' a_km1_ch2_q3
        , AddAnswerEvent' a_km1_ch2_q3_aNo2
        , AddAnswerEvent' a_km1_ch2_q3_aYes2
        ]
  insertEventsToKmc context (U.toString (kmc ^. kmcdtoKmContainerUuid)) events
  recompileKnowledgeModel context (U.toString (kmc ^. kmcdtoKmContainerUuid))
  logState "MIGRATION (KnowledgeModel/KnowledgeModelContainer): ended"
