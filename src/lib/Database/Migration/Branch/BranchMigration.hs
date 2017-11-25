module Database.Migration.Branch.BranchMigration where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Api.Resources.Branch.BranchDTO
import Common.Context
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.Migration.Branch.Data.Event.Event
import Model.Event.Event
import Service.Branch.BranchService
import Service.KnowledgeModel.KnowledgeModelService

runMigration context dspConfig logState = do
  logState "MIGRATION (KnowledgeModel/Branch): started"
  deleteBranches context
  let branch =
        BranchDTO
        { _bdtoUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
        , _bdtoName = "Amsterdam KM"
        , _bdtoArtifactId = "amsterdam-km"
        , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
        }
  createBranch context branch
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
  insertEventsToBranch context (U.toString (branch ^. bdtoUuid)) events
  recompileKnowledgeModel context (U.toString (branch ^. bdtoUuid))
  logState "MIGRATION (KnowledgeModel/Branch): ended"
