module Wizard.Service.Report.ReportGenerator where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Util.Uuid
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Indication
import Wizard.Service.Report.Evaluator.Metric

computeChapterReport :: Bool -> Int -> [Metric] -> KnowledgeModel -> [Reply] -> Chapter -> ChapterReport
computeChapterReport levelsEnabled requiredLevel metrics km replies ch =
  ChapterReport
    { _chapterReportChapterUuid = ch ^. uuid
    , _chapterReportIndications = computeIndications levelsEnabled requiredLevel km replies ch
    , _chapterReportMetrics = computeMetrics metrics km replies ch
    }

generateReport :: Int -> [Metric] -> KnowledgeModel -> [Reply] -> AppContextM Report
generateReport requiredLevel metrics km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  appConfig <- asks _appContextApplicationConfig
  let _levelsEnabled = appConfig ^. general . levelsEnabled
  return
    Report
      { _reportUuid = rUuid
      , _reportChapterReports =
          (computeChapterReport _levelsEnabled requiredLevel metrics km replies) <$> (getChaptersForKmUuid km)
      , _reportCreatedAt = now
      , _reportUpdatedAt = now
      }
