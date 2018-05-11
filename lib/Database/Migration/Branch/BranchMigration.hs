module Database.Migration.Branch.BranchMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Api.Resource.Branch.BranchDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.Migration.Branch.Data.Event.Event
import LensesConfig
import Model.Event.Event
import Service.Branch.BranchService
import Service.KnowledgeModel.KnowledgeModelService

runMigration appContext = do
  $(logInfo) "MIGRATION (KnowledgeModel/Branch): started"
  let context = appContext ^. oldContext
  liftIO $ deleteBranches context
  let branch =
        BranchDTO
        { _bdtoUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
        , _bdtoName = "Amsterdam KM"
        , _bdtoGroupId = "elixir.nl.amsterdam"
        , _bdtoArtifactId = "amsterdam-km"
        , _bdtoParentPackageId = Just "elixir.nl:core-nl:1.0.0"
        , _bdtoLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
        }
  liftIO $ createBranch context branch
  let events =
        [ AddQuestionEvent' a_km1_ch1_q1
        , AddQuestionEvent' a_km1_ch1_q2
        , AddAnswerEvent' a_km1_ch1_q2_aNo1
        , AddAnswerEvent' a_km1_ch1_q2_aYes1
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1
        , AddAnswerEvent' a_km1_ch1_q2_aNoFu1
        , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
        , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
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
  liftIO $ insertEventsToBranch context (U.toString (branch ^. bdtoUuid)) events
  liftIO $ recompileKnowledgeModel context (U.toString (branch ^. bdtoUuid))
  $(logInfo) "MIGRATION (KnowledgeModel/Branch): ended"
