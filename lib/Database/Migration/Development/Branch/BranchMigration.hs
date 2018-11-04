module Database.Migration.Development.Branch.BranchMigration where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Constant.Component
import Api.Resource.Branch.BranchDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.Migration.Development.Event.Data.Events
import LensesConfig
import Model.Event.Event
import Service.Branch.BranchService
import Service.KnowledgeModel.KnowledgeModelService
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(KnowledgeModel/Branch) started"
  deleteBranches
  let branch =
        BranchDTO
        { _branchDTOUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
        , _branchDTOName = "Amsterdam KM"
        , _branchDTOOrganizationId = "elixir.nl.amsterdam"
        , _branchDTOKmId = "amsterdam-km"
        , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
        , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
        }
  createBranch branch
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
  insertEventsToBranch (U.toString (branch ^. uuid)) events
  recompileKnowledgeModel (U.toString (branch ^. uuid))
  logInfo $ msg _CMP_MIGRATION "(KnowledgeModel/Branch) ended"
