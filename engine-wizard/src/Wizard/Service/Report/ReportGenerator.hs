module Wizard.Service.Report.ReportGenerator where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Util.Uuid
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Indication
import Wizard.Service.Report.Evaluator.Metric

computeChapterReport :: Maybe U.UUID -> KnowledgeModel -> [ReplyTuple] -> Chapter -> ChapterReport
computeChapterReport requiredPhaseUuid km replies ch =
  ChapterReport
    { chapterUuid = ch.uuid
    , indications = computeIndications requiredPhaseUuid km replies ch
    , metrics = computeMetrics km replies (Just ch)
    }

computeTotalReport :: Maybe U.UUID -> KnowledgeModel -> [ReplyTuple] -> TotalReport
computeTotalReport requiredPhaseUuid km replies =
  TotalReport
    { indications = computeTotalReportIndications requiredPhaseUuid km replies
    , metrics = computeMetrics km replies Nothing
    }

computeTotalReportIndications :: Maybe U.UUID -> KnowledgeModel -> [ReplyTuple] -> [Indication]
computeTotalReportIndications requiredPhaseUuid km replies =
  let chapterIndications = fmap (computeIndications requiredPhaseUuid km replies) (getChaptersForKmUuid km)
      mergeIndications [PhasesAnsweredIndication' (PhasesAnsweredIndication a1 b1), AnsweredIndication' (AnsweredIndication c1 d1)] [PhasesAnsweredIndication' (PhasesAnsweredIndication a2 b2), AnsweredIndication' (AnsweredIndication c2 d2)] =
        [ PhasesAnsweredIndication' (PhasesAnsweredIndication (a1 + a2) (b1 + b2))
        , AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))
        ]
      mergeIndications [AnsweredIndication' (AnsweredIndication c1 d1)] [AnsweredIndication' (AnsweredIndication c2 d2)] =
        [AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))]
   in foldl
        mergeIndications
        [PhasesAnsweredIndication' (PhasesAnsweredIndication 0 0), AnsweredIndication' (AnsweredIndication 0 0)]
        chapterIndications

generateReport :: Maybe U.UUID -> KnowledgeModel -> [ReplyTuple] -> AppContextM Report
generateReport requiredPhaseUuid km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  return
    Report
      { uuid = rUuid
      , totalReport = computeTotalReport requiredPhaseUuid km replies
      , chapterReports = computeChapterReport requiredPhaseUuid km replies <$> getChaptersForKmUuid km
      , createdAt = now
      , updatedAt = now
      }
