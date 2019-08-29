module Service.Report.ReportGenerator where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
import Model.Context.AppContext
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Service.Report.Evaluator.Indication
import Service.Report.Evaluator.Metric
import Util.Uuid

computeChapterReport :: Int -> [Metric] -> KnowledgeModel -> [Reply] -> Chapter -> ChapterReport
computeChapterReport requiredLevel metrics km replies ch =
  ChapterReport
  { _chapterReportChapterUuid = ch ^. uuid
  , _chapterReportIndications = computeIndications requiredLevel km replies ch
  , _chapterReportMetrics = computeMetrics metrics km replies ch
  }

generateReport :: Int -> [Metric] -> KnowledgeModel -> [Reply] -> AppContextM Report
generateReport requiredLevel metrics km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  return
    Report
    { _reportUuid = rUuid
    , _reportChapterReports = (computeChapterReport requiredLevel metrics km replies) <$> (getChaptersForKmUuid km)
    , _reportCreatedAt = now
    , _reportUpdatedAt = now
    }
