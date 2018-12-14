module Database.Migration.Development.Branch.BranchMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import Constant.Component
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.Migration.Development.Branch.Data.Branches
import Database.Migration.Development.Event.Data.Events
import LensesConfig
import Model.Event.Event
import Service.Branch.BranchService
import Service.KnowledgeModel.KnowledgeModelService
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(KnowledgeModel/Branch) started"
  deleteBranches
  createBranchWithParams (amsterdamBranch ^. uuid) (amsterdamBranch ^. createdAt) amsterdamBranchChange
  let events =
        [ AddQuestionEvent' a_km1_ch1_q1
        , AddQuestionEvent' a_km1_ch1_q2
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
        , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
        , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
        , AddExpertEvent' a_km1_ch1_q2_eAlbert
        , AddExpertEvent' a_km1_ch1_q2_eNikola
        , AddReferenceEvent' a_km1_ch1_q2_rCh1
        , AddReferenceEvent' a_km1_ch1_q2_rCh2
        , AddChapterEvent' a_km1_ch2
        , AddQuestionEvent' a_km1_ch2_q3
        , AddAnswerEvent' a_km1_ch2_q3_aNo2
        , AddAnswerEvent' a_km1_ch2_q3_aYes2
        ]
  insertEventsToBranch (U.toString $ amsterdamBranch ^. uuid) events
  recompileKnowledgeModel (U.toString $ amsterdamBranch ^. uuid)
  logInfo $ msg _CMP_MIGRATION "(KnowledgeModel/Branch) ended"
