module Wizard.Service.Report.ReportGenerator where

import Control.Monad.Reader (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Indication
import Wizard.Service.Report.Evaluator.Metric

computeChapterReport :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> Chapter -> ChapterReport
computeChapterReport requiredPhaseUuid km replies ch =
  ChapterReport
    { chapterUuid = ch.uuid
    , indications = computeIndications requiredPhaseUuid km replies ch
    , metrics = computeMetrics km replies (Just ch)
    }

computeTotalReport :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> TotalReport
computeTotalReport requiredPhaseUuid km replies =
  TotalReport
    { indications = computeTotalReportIndications requiredPhaseUuid km replies
    , metrics = computeMetrics km replies Nothing
    }

computeTotalReportIndications :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> [Indication]
computeTotalReportIndications requiredPhaseUuid km replies =
  let chapterIndications = fmap (computeIndications requiredPhaseUuid km replies) (getChaptersForKmUuid km)
      mergeIndications [PhasesAnsweredIndication' (PhasesAnsweredIndication a1 b1), AnsweredIndication' (AnsweredIndication c1 d1)] [PhasesAnsweredIndication' (PhasesAnsweredIndication a2 b2), AnsweredIndication' (AnsweredIndication c2 d2)] =
        [ PhasesAnsweredIndication' (PhasesAnsweredIndication (a1 + a2) (b1 + b2))
        , AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))
        ]
      mergeIndications [AnsweredIndication' (AnsweredIndication c1 d1)] [AnsweredIndication' (AnsweredIndication c2 d2)] =
        [AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))]
      mergeIndications _ _ = []
   in foldl
        mergeIndications
        [PhasesAnsweredIndication' (PhasesAnsweredIndication 0 0), AnsweredIndication' (AnsweredIndication 0 0)]
        chapterIndications

generateReport :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> AppContextM Report
generateReport requiredPhaseUuid km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  return
    Report
      { uuid = rUuid
      , totalReport = computeTotalReport requiredPhaseUuid km replies
      , chapterReports = computeChapterReport requiredPhaseUuid km replies <$> getChaptersForKmUuid km
      , chapters = M.elems km.entities.chapters
      , metrics = M.elems km.entities.metrics
      , createdAt = now
      , updatedAt = now
      }
